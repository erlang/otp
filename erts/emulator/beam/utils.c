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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define ERTS_DO_INCL_GLB_INLINE_FUNC_DEF

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "big.h"
#include "bif.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include "packet_parser.h"
#include "erl_gc.h"
#define ERTS_WANT_DB_INTERNAL__
#include "erl_db.h"
#include "erl_threads.h"
#include "register.h"
#include "dist.h"
#include "erl_printf.h"
#include "erl_threads.h"
#include "erl_smp.h"
#include "erl_time.h"

#undef M_TRIM_THRESHOLD
#undef M_TOP_PAD
#undef M_MMAP_THRESHOLD
#undef M_MMAP_MAX

#if !defined(ELIB_ALLOC_IS_CLIB) && defined(__GLIBC__) && defined(HAVE_MALLOC_H)
#include <malloc.h>
#endif

#if defined(ELIB_ALLOC_IS_CLIB) || !defined(HAVE_MALLOPT)
#undef  HAVE_MALLOPT
#define HAVE_MALLOPT 0
#endif

/* profile_scheduler mini message queue */

#ifdef ERTS_TIMER_THREAD
/* A timer thread is not welcomed with this lock violation work around.
 * - Björn-Egil
 */
#error Timer thread may not be enabled due to lock violation.
#endif

typedef struct {
    Uint scheduler_id;
    Uint no_schedulers;
    Uint Ms;
    Uint s;
    Uint us;
    Eterm state;
} profile_sched_msg;

typedef struct {
    profile_sched_msg msg[2];
    Uint n;
} profile_sched_msg_q;

#ifdef ERTS_SMP

static void 
dispatch_profile_msg_q(profile_sched_msg_q *psmq)
{
    int i = 0;
    profile_sched_msg *msg = NULL;
    ASSERT(psmq != NULL);
    for (i = 0; i < psmq->n; i++) {
        msg = &(psmq->msg[i]);
	profile_scheduler_q(make_small(msg->scheduler_id), msg->state, am_undefined, msg->Ms, msg->s, msg->us);
    }
}

#endif


Eterm*
erts_heap_alloc(Process* p, Uint need)
{
    ErlHeapFragment* bp;
    Eterm* htop;
    Uint n;
#if defined(DEBUG) || defined(CHECK_FOR_HOLES)
    Uint i;
#endif

#ifdef FORCE_HEAP_FRAGS
    if (p->space_verified && p->space_verified_from!=NULL
	&& HEAP_TOP(p) >= p->space_verified_from
	&& HEAP_TOP(p) + need <= p->space_verified_from + p->space_verified
	&& HEAP_LIMIT(p) - HEAP_TOP(p) >= need) {
	
	Uint consumed = need + (HEAP_TOP(p) - p->space_verified_from);
	ASSERT(consumed <= p->space_verified);
	p->space_verified -= consumed;
	p->space_verified_from += consumed;
	HEAP_TOP(p) = p->space_verified_from;
	return HEAP_TOP(p) - need;
    }
    p->space_verified = 0;
    p->space_verified_from = NULL;
#endif /* FORCE_HEAP_FRAGS */

    n = need;
    bp = MBUF(p);
    if (bp != NULL && need <= (bp->alloc_size - bp->used_size)) {
	Eterm* ret = bp->mem + bp->used_size;
	bp->used_size += need;
	return ret;
    }
#ifdef DEBUG
    n++;
#endif
    bp = (ErlHeapFragment*)
	ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP_FRAG, ERTS_HEAP_FRAG_SIZE(n));

#if defined(DEBUG) || defined(CHECK_FOR_HOLES)
    for (i = 0; i < n; i++) {
	bp->mem[i] = ERTS_HOLE_MARKER;
    }
#endif

#ifdef DEBUG
    n--;
#endif

    /*
     * When we have created a heap fragment, we are no longer allowed
     * to store anything more on the heap. 
     */
    htop = HEAP_TOP(p);
    if (htop < HEAP_LIMIT(p)) {
	*htop = make_pos_bignum_header(HEAP_LIMIT(p)-htop-1);
	HEAP_TOP(p) = HEAP_LIMIT(p);
    }

    bp->next = MBUF(p);
    MBUF(p) = bp;
    bp->alloc_size = n;
    bp->used_size = n;
    MBUF_SIZE(p) += n;
    bp->off_heap.mso = NULL;
#ifndef HYBRID /* FIND ME! */
    bp->off_heap.funs = NULL;
#endif
    bp->off_heap.externals = NULL;
    bp->off_heap.overhead = 0;

    return bp->mem;
}

#ifdef CHECK_FOR_HOLES
Eterm*
erts_set_hole_marker(Eterm* ptr, Uint sz)
{
    Eterm* p = ptr;
    int i;

    for (i = 0; i < sz; i++) {
	*p++ = ERTS_HOLE_MARKER;
    }
    return ptr;
}
#endif

/*
 * Helper function for the ESTACK macros defined in global.h.
 */
void
erl_grow_stack(Eterm** start, Eterm** sp, Eterm** end)
{
    Uint old_size = (*end - *start);
    Uint new_size = old_size * 2;
    Uint sp_offs = *sp - *start;
    if (new_size > 2 * DEF_ESTACK_SIZE) {
	*start = erts_realloc(ERTS_ALC_T_ESTACK, (void *) *start, new_size*sizeof(Eterm));
    } else {
	Eterm* new_ptr = erts_alloc(ERTS_ALC_T_ESTACK, new_size*sizeof(Eterm));
	sys_memcpy(new_ptr, *start, old_size*sizeof(Eterm));
	*start = new_ptr;
    }
    *end = *start + new_size;
    *sp = *start + sp_offs;
}
/*
 * Helper function for the ESTACK macros defined in global.h.
 */
void
erl_grow_wstack(UWord** start, UWord** sp, UWord** end)
{
    Uint old_size = (*end - *start);
    Uint new_size = old_size * 2;
    Uint sp_offs = *sp - *start;
    if (new_size > 2 * DEF_ESTACK_SIZE) {
	*start = erts_realloc(ERTS_ALC_T_ESTACK, (void *) *start, new_size*sizeof(UWord));
    } else {
	UWord* new_ptr = erts_alloc(ERTS_ALC_T_ESTACK, new_size*sizeof(UWord));
	sys_memcpy(new_ptr, *start, old_size*sizeof(UWord));
	*start = new_ptr;
    }
    *end = *start + new_size;
    *sp = *start + sp_offs;
}

/* CTYPE macros */

#define LATIN1

#define IS_DIGIT(c)  ((c) >= '0' && (c) <= '9')
#ifdef LATIN1
#define IS_LOWER(c)  (((c) >= 'a' && (c) <= 'z') \
		      || ((c) >= 128+95 && (c) <= 255 && (c) != 247))
#define IS_UPPER(c)  (((c) >= 'A' && (c) <= 'Z') \
		      || ((c) >= 128+64 && (c) <= 128+94 && (c) != 247-32))
#else
#define IS_LOWER(c)  ((c) >= 'a' && (c) <= 'z')
#define IS_UPPER(c)  ((c) >= 'A' && (c) <= 'Z')
#endif

#define IS_ALNUM(c)  (IS_DIGIT(c) || IS_LOWER(c) || IS_UPPER(c))

/* We don't include 160 (non-breaking space). */
#define IS_SPACE(c)  (c == ' ' || c == '\n' || c == '\t' || c == '\r')

#ifdef LATIN1
#define IS_CNTRL(c)  ((c) < ' ' || (c) == 127 \
		      || ((c) >= 128 && (c) < 128+32))
#else
/* Treat all non-ASCII as control characters */
#define IS_CNTRL(c)  ((c) < ' ' || (c) >= 127)
#endif

#define IS_PRINT(c)  (!IS_CNTRL(c))

/*
 * Calculate length of a list.
 * Returns -1 if not a proper list (i.e. not terminated with NIL)
 */
int
list_length(Eterm list)
{
    int i = 0;

    while(is_list(list)) {
	i++;
	list = CDR(list_val(list));
    }
    if (is_not_nil(list)) {
	return -1;
    }
    return i;
}

Uint erts_fit_in_bits(Uint n)
{
   Uint i;

   i = 0;
   while (n > 0) {
      i++;
      n >>= 1;
   }
   return i;
}

int
erts_print(int to, void *arg, char *format, ...)
{
    int res;
    va_list arg_list;
    va_start(arg_list, format);

    if (to < ERTS_PRINT_MIN)
	res = -EINVAL;
    else {
	switch (to) {
	case ERTS_PRINT_STDOUT:
	    res = erts_vprintf(format, arg_list);
	    break;
	case ERTS_PRINT_STDERR:
	    res = erts_vfprintf(stderr, format, arg_list);
	    break;
	case ERTS_PRINT_FILE:
	    res = erts_vfprintf((FILE *) arg, format, arg_list);
	    break;
	case ERTS_PRINT_SBUF:
	    res = erts_vsprintf((char *) arg, format, arg_list);
	    break;
	case ERTS_PRINT_SNBUF:
	    res = erts_vsnprintf(((erts_print_sn_buf *) arg)->buf,
				 ((erts_print_sn_buf *) arg)->size,
				 format,
				 arg_list);
	    break;
	case ERTS_PRINT_DSBUF:
	    res = erts_vdsprintf((erts_dsprintf_buf_t *) arg, format, arg_list);
	    break;
	case ERTS_PRINT_INVALID:
	    res = -EINVAL;
	    break;
	default:
	    res = erts_vfdprintf((int) to, format, arg_list);
	    break;
	}
    }

    va_end(arg_list);
    return res;
}

int
erts_putc(int to, void *arg, char c)
{
    return erts_print(to, arg, "%c", c);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Some Erlang term building utility functions (to be used when performance  *
 * isn't critical).                                                          *
 *                                                                           *
 * Add more functions like these here (and function prototypes in global.h)  *
 * when needed.                                                              *
 *                                                                           *
\*                                                                           */

Eterm
erts_bld_atom(Uint **hpp, Uint *szp, char *str)
{
    if (hpp)
	return am_atom_put(str, sys_strlen(str));
    else
	return THE_NON_VALUE;
}

Eterm
erts_bld_uint(Uint **hpp, Uint *szp, Uint ui)
{
    Eterm res = THE_NON_VALUE;
    if (IS_USMALL(0, ui)) {
	if (hpp)
	    res = make_small(ui);
    }
    else {
	if (szp)
	    *szp += BIG_UINT_HEAP_SIZE;
	if (hpp) {
	    res = uint_to_big(ui, *hpp);
	    *hpp += BIG_UINT_HEAP_SIZE;
	}
    }
    return res;
}

/*
 * Erts_bld_uword is more or less similar to erts_bld_uint, but a pointer
 * can safely be passed.
 */

Eterm
erts_bld_uword(Uint **hpp, Uint *szp, UWord uw)
{
    Eterm res = THE_NON_VALUE;
    if (IS_USMALL(0, uw)) {
	if (hpp)
	    res = make_small((Uint) uw);
    }
    else {
	if (szp)
	    *szp += BIG_UWORD_HEAP_SIZE(uw);
	if (hpp) {
	    res = uword_to_big(uw, *hpp);
	    *hpp += BIG_UWORD_HEAP_SIZE(uw);
	}
    }
    return res;
}


Eterm
erts_bld_uint64(Uint **hpp, Uint *szp, Uint64 ui64)
{
    Eterm res = THE_NON_VALUE;
    if (IS_USMALL(0, ui64)) {
	if (hpp)
	    res = make_small((Uint) ui64);
    }
    else {
	if (szp)
	    *szp = ERTS_UINT64_HEAP_SIZE(ui64);
	if (hpp)
	    res = erts_uint64_to_big(ui64, hpp);
    }
    return res;
}

Eterm
erts_bld_sint64(Uint **hpp, Uint *szp, Sint64 si64)
{
    Eterm res = THE_NON_VALUE;
    if (IS_SSMALL(si64)) {
	if (hpp)
	    res = make_small((Sint) si64);
    }
    else {
	if (szp)
	    *szp = ERTS_SINT64_HEAP_SIZE(si64);
	if (hpp)
	    res = erts_sint64_to_big(si64, hpp);
    }
    return res;
}


Eterm
erts_bld_cons(Uint **hpp, Uint *szp, Eterm car, Eterm cdr)
{
    Eterm res = THE_NON_VALUE;
    if (szp)
	*szp += 2;
    if (hpp) {
	res = CONS(*hpp, car, cdr);
	*hpp += 2;
    }
    return res;
}

Eterm
erts_bld_tuple(Uint **hpp, Uint *szp, Uint arity, ...)
{
    Eterm res = THE_NON_VALUE;

    ASSERT(arity < (((Uint)1) << (sizeof(Uint)*8 - _HEADER_ARITY_OFFS)));

    if (szp)
	*szp += arity + 1;
    if (hpp) {
	res = make_tuple(*hpp);
	*((*hpp)++) = make_arityval(arity);

	if (arity > 0) {
	    Uint i;
	    va_list argp;

	    va_start(argp, arity);
	    for (i = 0; i < arity; i++) {
                *((*hpp)++) = va_arg(argp, Eterm);
            }
	    va_end(argp);
	}
    }
    return res;
}


Eterm erts_bld_tuplev(Uint **hpp, Uint *szp, Uint arity, Eterm terms[])
{
    Eterm res = THE_NON_VALUE;
    /*
     * Note callers expect that 'terms' is *not* accessed if hpp == NULL.
     */

    ASSERT(arity < (((Uint)1) << (sizeof(Uint)*8 - _HEADER_ARITY_OFFS)));

    if (szp)
	*szp += arity + 1;
    if (hpp) {

	res = make_tuple(*hpp);
	*((*hpp)++) = make_arityval(arity);

	if (arity > 0) {
	    Uint i;
	    for (i = 0; i < arity; i++)
		*((*hpp)++) = terms[i];
	}
    }
    return res;
}

Eterm
erts_bld_string_n(Uint **hpp, Uint *szp, const char *str, Sint len)
{
    Eterm res = THE_NON_VALUE;
    Sint i = len;
    if (szp)
	*szp += len*2;
    if (hpp) {
	res = NIL;
	while (--i >= 0) {
	    res = CONS(*hpp, make_small((byte) str[i]), res);
	    *hpp += 2;
	}
    }
    return res;
}

Eterm
erts_bld_list(Uint **hpp, Uint *szp, Sint length, Eterm terms[])
{
    Eterm list = THE_NON_VALUE;
    if (szp)
	*szp += 2*length;
    if (hpp) {
	Sint i = length;
	list = NIL;

	while (--i >= 0) {
	    list = CONS(*hpp, terms[i], list);
	    *hpp += 2;
	}
    }
    return list;
}

Eterm
erts_bld_2tup_list(Uint **hpp, Uint *szp,
		   Sint length, Eterm terms1[], Uint terms2[])
{
    Eterm res = THE_NON_VALUE;
    if (szp)
	*szp += 5*length;
    if (hpp) {
	Sint i = length;
	res = NIL;

	while (--i >= 0) {
	    res = CONS(*hpp+3, TUPLE2(*hpp, terms1[i], terms2[i]), res);
	    *hpp += 5;
	}
    }
    return res;
}

Eterm
erts_bld_atom_uint_2tup_list(Uint **hpp, Uint *szp,
			     Sint length, Eterm atoms[], Uint uints[])
{
    Sint i;
    Eterm res = THE_NON_VALUE;
    if (szp) {
	*szp += 5*length;
	i = length;
	while (--i >= 0) {
	    if (!IS_USMALL(0, uints[i]))
		*szp += BIG_UINT_HEAP_SIZE;
	}
    }
    if (hpp) {
	i = length;
	res = NIL;

	while (--i >= 0) {
	    Eterm ui;

	    if (IS_USMALL(0, uints[i]))
		ui = make_small(uints[i]);
	    else {
		ui = uint_to_big(uints[i], *hpp);
		*hpp += BIG_UINT_HEAP_SIZE;
	    }
	    
	    res = CONS(*hpp+3, TUPLE2(*hpp, atoms[i], ui), res);
	    *hpp += 5;
	}
    }
    return res;
}

Eterm
erts_bld_atom_2uint_3tup_list(Uint **hpp, Uint *szp, Sint length,
			      Eterm atoms[], Uint uints1[], Uint uints2[])
{
    Sint i;
    Eterm res = THE_NON_VALUE;
    if (szp) {
	*szp += 6*length;
	i = length;
	while (--i >= 0) {
	    if (!IS_USMALL(0, uints1[i]))
		*szp += BIG_UINT_HEAP_SIZE;
	    if (!IS_USMALL(0, uints2[i]))
		*szp += BIG_UINT_HEAP_SIZE;
	}
    }
    if (hpp) {
	i = length;
	res = NIL;

	while (--i >= 0) {
	    Eterm ui1;
	    Eterm ui2;

	    if (IS_USMALL(0, uints1[i]))
		ui1 = make_small(uints1[i]);
	    else {
		ui1 = uint_to_big(uints1[i], *hpp);
		*hpp += BIG_UINT_HEAP_SIZE;
	    }
	    
	    if (IS_USMALL(0, uints2[i]))
		ui2 = make_small(uints2[i]);
	    else {
		ui2 = uint_to_big(uints2[i], *hpp);
		*hpp += BIG_UINT_HEAP_SIZE;
	    }
	    
	    res = CONS(*hpp+4, TUPLE3(*hpp, atoms[i], ui1, ui2), res);
	    *hpp += 6;
	}
    }
    return res;
}

/*                                                                           *\
 *                                                                           *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* make a hash index from an erlang term */

/*
** There are three hash functions.
** make_broken_hash: the one used for backward compatibility
** is called from the bif erlang:hash/2. Should never be used
** as it a) hashes only a part of binaries, b) hashes bignums really poorly,
** c) hashes bignums differently on different endian processors and d) hashes 
** small integers with different weights on different bytes.
**
** make_hash: A hash function that will give the same values for the same
** terms regardless of the internal representation. Small integers are 
** hashed using the same algorithm as bignums and bignums are hashed 
** independent of the CPU endianess. 
** Make_hash also hashes pids, ports and references like 32 bit numbers 
** (but with different constants). 
** make_hash() is called from the bif erlang:phash/2
**
** The idea behind the hash algorithm is to produce values suitable for 
** linear dynamic hashing. We cannot choose the range at all while hashing 
** (it's not even supplied to the hashing functions). The good old algorithm
** [H = H*C+X mod M, where H is the hash value, C is a "random" constant(or M),
** M is the range, preferably a prime, and X is each byte value] is therefore 
** modified to:
** H = H*C+X mod 2^32, where C is a large prime. This gives acceptable 
** "spreading" of the hashes, so that later modulo calculations also will give
** acceptable "spreading" in the range. 
** We really need to hash on bytes, otherwise the 
** upper bytes of a word will be less significant than the lower ones. That's 
** not acceptable at all. For internal use one could maybe optimize by using
** another hash function, that is less strict but faster. That is, however, not
** implemented.
**
** Short semi-formal description of make_hash:
**
** In make_hash, the number N is treated like this:
**  Abs(N) is hashed bytewise with the least significant byte, B(0), first.
**  The number of bytes (J) to calculate hash on in N is 
**  (the number of _32_ bit words needed to store the unsigned 
**   value of abs(N)) * 4.
**  X = FUNNY_NUMBER2
**  If N < 0, Y = FUNNY_NUMBER4 else Y = FUNNY_NUMBER3.
**  The hash value is Y*h(J) mod 2^32 where h(J) is calculated like
**  h(0) = <initial hash> 
**  h(i) = h(i-i)*X + B(i-1)
** The above should hold regardless of internal representation.
** Pids are hashed like small numbers but with differrent constants, as are
** ports.
** References are hashed like ports but only on the least significant byte.
** Binaries are hashed on all bytes (not on the 15 first as in 
** make_broken_hash()).
** Bytes in lists (possibly text strings) use a simpler multiplication inlined
** in the handling of lists, that is an optimization.
** Everything else is like in the old hash (make_broken_hash()).
**
** make_hash2() is faster than make_hash, in particular for bignums
** and binaries, and produces better hash values. 
*/

/* some prime numbers just above 2 ^ 28 */

#define FUNNY_NUMBER1  268440163
#define FUNNY_NUMBER2  268439161
#define FUNNY_NUMBER3  268435459
#define FUNNY_NUMBER4  268436141
#define FUNNY_NUMBER5  268438633
#define FUNNY_NUMBER6  268437017
#define FUNNY_NUMBER7  268438039
#define FUNNY_NUMBER8  268437511
#define FUNNY_NUMBER9  268439627
#define FUNNY_NUMBER10 268440479
#define FUNNY_NUMBER11 268440577
#define FUNNY_NUMBER12 268440581

static Uint32
hash_binary_bytes(Eterm bin, Uint sz, Uint32 hash)
{
    byte* ptr;
    Uint bitoffs;
    Uint bitsize;

    ERTS_GET_BINARY_BYTES(bin, ptr, bitoffs, bitsize);
    if (bitoffs == 0) {
	while (sz--) {
	    hash = hash*FUNNY_NUMBER1 + *ptr++;
	}
	if (bitsize > 0) {
	    byte b = *ptr;

	    b >>= 8 - bitsize;
	    hash = (hash*FUNNY_NUMBER1 + b) * FUNNY_NUMBER12 + bitsize;
	}
    } else {
	Uint previous = *ptr++;
	Uint b;
	Uint lshift = bitoffs;
	Uint rshift = 8 - lshift;
	    
	while (sz--) {
	    b = (previous << lshift) & 0xFF;
	    previous = *ptr++;
	    b |= previous >> rshift;
	    hash = hash*FUNNY_NUMBER1 + b;
	}
	if (bitsize > 0) {
	    b = (previous << lshift) & 0xFF;
	    previous = *ptr++;
	    b |= previous >> rshift;
	    
	    b >>= 8 - bitsize;
	    hash = (hash*FUNNY_NUMBER1 + b) * FUNNY_NUMBER12 + bitsize;
	}
    }
    return hash;
}

Uint32 make_hash(Eterm term_arg)
{
    DECLARE_WSTACK(stack);
    Eterm term = term_arg;
    Eterm hash = 0;
    unsigned op;

    /* Must not collide with the real tag_val_def's: */
#define MAKE_HASH_TUPLE_OP 0x10
#define MAKE_HASH_FUN_OP 0x11
#define MAKE_HASH_CDR_PRE_OP 0x12
#define	MAKE_HASH_CDR_POST_OP 0x13

    /* 
    ** Convenience macro for calculating a bytewise hash on an unsigned 32 bit 
    ** integer.
    ** If the endianess is known, we could be smarter here, 
    ** but that gives no significant speedup (on a sparc at least) 
    */
#define UINT32_HASH_STEP(Expr, Prime1)					\
	do {								\
	    Uint32 x = (Uint32) (Expr);	                                \
	    hash =							\
		(((((hash)*(Prime1) + (x & 0xFF)) * (Prime1) + 	        \
		((x >> 8) & 0xFF)) * (Prime1) + 			\
		((x >> 16) & 0xFF)) * (Prime1) + 			\
		 (x >> 24));						\
	} while(0)

#define UINT32_HASH_RET(Expr, Prime1, Prime2)   	\
	UINT32_HASH_STEP(Expr, Prime1);			\
        hash = hash * (Prime2);				\
        break		 
		
	    
    /* 
     * Significant additions needed for real 64 bit port with larger fixnums.
     */	    

    /* 
     * Note, for the simple 64bit port, not utilizing the 
     * larger word size this function will work without modification. 
     */
tail_recur:
    op = tag_val_def(term);

    for (;;) {
    switch (op) {
    case NIL_DEF:
	hash = hash*FUNNY_NUMBER3 + 1;
	break;
    case ATOM_DEF:
	hash = hash*FUNNY_NUMBER1 + 
	    (atom_tab(atom_val(term))->slot.bucket.hvalue);
	break;
    case SMALL_DEF:
	{
	    Sint y1 = signed_val(term);
	    Uint y2 = y1 < 0 ? -(Uint)y1 : y1;

	    UINT32_HASH_STEP(y2, FUNNY_NUMBER2);
#if defined(ARCH_64) && !HALFWORD_HEAP
	    if (y2 >> 32)
		UINT32_HASH_STEP(y2 >> 32, FUNNY_NUMBER2);
#endif
	    hash *= (y1 < 0 ? FUNNY_NUMBER4 : FUNNY_NUMBER3);
	    break;
	}
    case BINARY_DEF:
	{
	    Uint sz = binary_size(term);

	    hash = hash_binary_bytes(term, sz, hash);
	    hash = hash*FUNNY_NUMBER4 + sz;
	    break;
	}
    case EXPORT_DEF:
	{
	    Export* ep = *((Export **) (export_val(term) + 1));

	    hash = hash * FUNNY_NUMBER11 + ep->code[2];
	    hash = hash*FUNNY_NUMBER1 + 
		(atom_tab(atom_val(ep->code[0]))->slot.bucket.hvalue);
	    hash = hash*FUNNY_NUMBER1 + 
		(atom_tab(atom_val(ep->code[1]))->slot.bucket.hvalue);
	    break;
	}

    case FUN_DEF:
	{
	    ErlFunThing* funp = (ErlFunThing *) fun_val(term);
	    Uint num_free = funp->num_free;

	    hash = hash * FUNNY_NUMBER10 + num_free;
	    hash = hash*FUNNY_NUMBER1 + 
		(atom_tab(atom_val(funp->fe->module))->slot.bucket.hvalue);
	    hash = hash*FUNNY_NUMBER2 + funp->fe->old_index;
	    hash = hash*FUNNY_NUMBER2 + funp->fe->old_uniq;
	    if (num_free > 0) {
		if (num_free > 1) {
		    WSTACK_PUSH3(stack, (UWord) &funp->env[1], (num_free-1), MAKE_HASH_FUN_OP);
		}
		term = funp->env[0];
		goto tail_recur;
	    }
	    break;
	}
    case PID_DEF:
	UINT32_HASH_RET(internal_pid_number(term),FUNNY_NUMBER5,FUNNY_NUMBER6);
    case EXTERNAL_PID_DEF:
	UINT32_HASH_RET(external_pid_number(term),FUNNY_NUMBER5,FUNNY_NUMBER6);
    case PORT_DEF:
	UINT32_HASH_RET(internal_port_number(term),FUNNY_NUMBER9,FUNNY_NUMBER10);
    case EXTERNAL_PORT_DEF:
	UINT32_HASH_RET(external_port_number(term),FUNNY_NUMBER9,FUNNY_NUMBER10);
    case REF_DEF:
	UINT32_HASH_RET(internal_ref_numbers(term)[0],FUNNY_NUMBER9,FUNNY_NUMBER10);
    case EXTERNAL_REF_DEF:
	UINT32_HASH_RET(external_ref_numbers(term)[0],FUNNY_NUMBER9,FUNNY_NUMBER10);
    case FLOAT_DEF: 
	{
	    FloatDef ff;
	    GET_DOUBLE(term, ff);
	    hash = hash*FUNNY_NUMBER6 + (ff.fw[0] ^ ff.fw[1]);
	    break;
	}

    case MAKE_HASH_CDR_PRE_OP:
	term = (Eterm) WSTACK_POP(stack);
	if (is_not_list(term)) {
	    WSTACK_PUSH(stack, (UWord) MAKE_HASH_CDR_POST_OP);
	    goto tail_recur;
	}
	/* fall through */
    case LIST_DEF:
	{
	    Eterm* list = list_val(term);
	    while(is_byte(*list)) {
		/* Optimization for strings. 
		** Note that this hash is different from a 'small' hash,
		** as multiplications on a Sparc is so slow.
		*/
		hash = hash*FUNNY_NUMBER2 + unsigned_val(*list);
		
		if (is_not_list(CDR(list))) {
		    WSTACK_PUSH(stack, MAKE_HASH_CDR_POST_OP);
		    term = CDR(list);
		    goto tail_recur;
		}		
		list = list_val(CDR(list));
	    }
	    WSTACK_PUSH2(stack, CDR(list), MAKE_HASH_CDR_PRE_OP);
	    term = CAR(list);
	    goto tail_recur;
	}
    case MAKE_HASH_CDR_POST_OP:
	hash *= FUNNY_NUMBER8;
	break;

    case BIG_DEF:
	/* Note that this is the exact same thing as the hashing of smalls.*/
	{
	    Eterm* ptr  = big_val(term);
	    Uint n = BIG_SIZE(ptr);
	    Uint k = n-1;
	    ErtsDigit d;
	    int is_neg = BIG_SIGN(ptr);
	    Uint i;
	    int j;

	    for (i = 0; i < k; i++)  {
		d = BIG_DIGIT(ptr, i);
		for(j = 0; j < sizeof(ErtsDigit); ++j) {
		    hash = (hash*FUNNY_NUMBER2) + (d & 0xff);
		    d >>= 8;
		}
	    }
	    d = BIG_DIGIT(ptr, k);
	    k = sizeof(ErtsDigit);
#if defined(ARCH_64) && !HALFWORD_HEAP
	    if (!(d >> 32))
		k /= 2;
#endif
	    for(j = 0; j < (int)k; ++j) {
		hash = (hash*FUNNY_NUMBER2) + (d & 0xff);
		d >>= 8;
	    }
	    hash *= is_neg ? FUNNY_NUMBER4 : FUNNY_NUMBER3;
	    break;
	}	
    case TUPLE_DEF: 
	{
	    Eterm* ptr = tuple_val(term);
	    Uint arity = arityval(*ptr);

	    WSTACK_PUSH3(stack, (UWord) arity, (UWord)(ptr+1), (UWord) arity);
	    op = MAKE_HASH_TUPLE_OP;	    
	}/*fall through*/
    case MAKE_HASH_TUPLE_OP:
    case MAKE_HASH_FUN_OP:
	{
	    Uint i = (Uint) WSTACK_POP(stack);
	    Eterm* ptr = (Eterm*) WSTACK_POP(stack);
	    if (i != 0) {
		term = *ptr;
		WSTACK_PUSH3(stack, (UWord)(ptr+1), (UWord) i-1, (UWord) op);
		goto tail_recur;
	    }
	    if (op == MAKE_HASH_TUPLE_OP) {
		Uint32 arity = (Uint32) WSTACK_POP(stack);
		hash = hash*FUNNY_NUMBER9 + arity;
	    }
	    break;
	}    
	
    default:
	erl_exit(1, "Invalid tag in make_hash(0x%X,0x%X)\n", term, op);
	return 0;
      }
      if (WSTACK_ISEMPTY(stack)) break;
      op = WSTACK_POP(stack);
    }
    DESTROY_WSTACK(stack);
    return hash;

#undef UINT32_HASH_STEP
#undef UINT32_HASH_RET
}



/* Hash function suggested by Bob Jenkins. */

#define MIX(a,b,c)                 \
do {                               \
  a -= b; a -= c; a ^= (c>>13);    \
  b -= c; b -= a; b ^= (a<<8);     \
  c -= a; c -= b; c ^= (b>>13);    \
  a -= b; a -= c; a ^= (c>>12);    \
  b -= c; b -= a; b ^= (a<<16);    \
  c -= a; c -= b; c ^= (b>>5);     \
  a -= b; a -= c; a ^= (c>>3);     \
  b -= c; b -= a; b ^= (a<<10);    \
  c -= a; c -= b; c ^= (b>>15);    \
} while(0)

#define HCONST 0x9e3779b9UL /* the golden ratio; an arbitrary value */

Uint32
block_hash(byte *k, unsigned length, Uint32 initval)
{
   Uint32 a,b,c;
   unsigned len;

   /* Set up the internal state */
   len = length;
   a = b = HCONST;
   c = initval;           /* the previous hash value */

   while (len >= 12)
   {
      a += (k[0] +((Uint32)k[1]<<8) +((Uint32)k[2]<<16) +((Uint32)k[3]<<24));
      b += (k[4] +((Uint32)k[5]<<8) +((Uint32)k[6]<<16) +((Uint32)k[7]<<24));
      c += (k[8] +((Uint32)k[9]<<8) +((Uint32)k[10]<<16)+((Uint32)k[11]<<24));
      MIX(a,b,c);
      k += 12; len -= 12;
   }

   c += length;
   switch(len)              /* all the case statements fall through */
   {
   case 11: c+=((Uint32)k[10]<<24);
   case 10: c+=((Uint32)k[9]<<16);
   case 9 : c+=((Uint32)k[8]<<8);
      /* the first byte of c is reserved for the length */
   case 8 : b+=((Uint32)k[7]<<24);
   case 7 : b+=((Uint32)k[6]<<16);
   case 6 : b+=((Uint32)k[5]<<8);
   case 5 : b+=k[4];
   case 4 : a+=((Uint32)k[3]<<24);
   case 3 : a+=((Uint32)k[2]<<16);
   case 2 : a+=((Uint32)k[1]<<8);
   case 1 : a+=k[0];
     /* case 0: nothing left to add */
   }
   MIX(a,b,c);
   return c;
}

Uint32
make_hash2(Eterm term)
{
    Uint32 hash;
    DeclareTmpHeapNoproc(tmp_big,2);

/* (HCONST * {2, ..., 14}) mod 2^32 */
#define HCONST_2 0x3c6ef372UL
#define HCONST_3 0xdaa66d2bUL
#define HCONST_4 0x78dde6e4UL
#define HCONST_5 0x1715609dUL
#define HCONST_6 0xb54cda56UL
#define HCONST_7 0x5384540fUL
#define HCONST_8 0xf1bbcdc8UL
#define HCONST_9 0x8ff34781UL
#define HCONST_10 0x2e2ac13aUL
#define HCONST_11 0xcc623af3UL
#define HCONST_12 0x6a99b4acUL
#define HCONST_13 0x08d12e65UL
#define HCONST_14 0xa708a81eUL
#define HCONST_15 0x454021d7UL

#define UINT32_HASH_2(Expr1, Expr2, AConst)       \
         do {                                     \
	    Uint32 a,b;                           \
	    a = AConst + (Uint32) (Expr1);        \
	    b = AConst + (Uint32) (Expr2);        \
	    MIX(a,b,hash);                        \
	 } while(0)

#define UINT32_HASH(Expr, AConst) UINT32_HASH_2(Expr, 0, AConst)

#define SINT32_HASH(Expr, AConst)                 \
	do {					  \
            Sint32 y = (Sint32) (Expr);           \
	    if (y < 0) {			  \
		UINT32_HASH(-y, AConst);          \
                /* Negative numbers are unnecessarily mixed twice. */ \
	    } 					  \
	    UINT32_HASH(y, AConst);          	  \
	} while(0)

#define IS_SSMALL28(x) (((Uint) (((x) >> (28-1)) + 1)) < 2)
    /* Optimization. Simple cases before declaration of estack. */
    if (primary_tag(term) == TAG_PRIMARY_IMMED1) {
	switch (term & _TAG_IMMED1_MASK) {
	case _TAG_IMMED1_IMMED2:
	    switch (term & _TAG_IMMED2_MASK) {
	    case _TAG_IMMED2_ATOM:
		/* Fast, but the poor hash value should be mixed. */
		return atom_tab(atom_val(term))->slot.bucket.hvalue;
	    }
	    break;
	case _TAG_IMMED1_SMALL:
	  {
	      Sint x = signed_val(term);

	      if (SMALL_BITS > 28 && !IS_SSMALL28(x)) {
		  term = small_to_big(x, tmp_big);
		  break;
	      }
	      hash = 0;
	      SINT32_HASH(x, HCONST);
	      return hash;
	  }
	}
    };
    {
    Eterm tmp;
    DECLARE_ESTACK(s);

    UseTmpHeapNoproc(2);
    hash = 0;
    for (;;) {
	switch (primary_tag(term)) {
	case TAG_PRIMARY_LIST:
	{
	    int c = 0;
	    Uint32 sh = 0;
	    Eterm* ptr = list_val(term);
	    while (is_byte(*ptr)) {
		/* Optimization for strings. */
		sh = (sh << 8) + unsigned_val(*ptr);
		if (c == 3) {
		    UINT32_HASH(sh, HCONST_4);
		    c = sh = 0;
		} else {
		    c++;
		}
		term = CDR(ptr);
		if (is_not_list(term))
		    break;
		ptr = list_val(term);
	    }
	    if (c > 0)
		UINT32_HASH(sh, HCONST_4);
	    if (is_list(term)) {
		term = *ptr;
		tmp = *++ptr;
		ESTACK_PUSH(s, tmp);	    
	    }
	}
	break;
	case TAG_PRIMARY_BOXED:
	{
	    Eterm hdr = *boxed_val(term);
	    ASSERT(is_header(hdr));
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG:
	    {
		int i;
		int arity = header_arity(hdr);
		Eterm* elem = tuple_val(term);
		UINT32_HASH(arity, HCONST_9);
		if (arity == 0) /* Empty tuple */ 
		    goto hash2_common;
		for (i = arity; i >= 2; i--) {
		    tmp = elem[i];
		    ESTACK_PUSH(s, tmp);
		}
		term = elem[1];
	    }
	    break;
	    case EXPORT_SUBTAG:
	    {
		Export* ep = *((Export **) (export_val(term) + 1));

		UINT32_HASH_2
		    (ep->code[2], 
		     atom_tab(atom_val(ep->code[0]))->slot.bucket.hvalue,
		     HCONST);
		UINT32_HASH
		    (atom_tab(atom_val(ep->code[1]))->slot.bucket.hvalue,
		     HCONST_14);
		goto hash2_common;
	    }

	    case FUN_SUBTAG:
	    {
		ErlFunThing* funp = (ErlFunThing *) fun_val(term);
		Uint num_free = funp->num_free;

		UINT32_HASH_2
		    (num_free, 
		     atom_tab(atom_val(funp->fe->module))->slot.bucket.hvalue,
		     HCONST);
		UINT32_HASH_2
		    (funp->fe->old_index, funp->fe->old_uniq, HCONST);
		if (num_free == 0) {
		    goto hash2_common;
		} else {
		    Eterm* bptr = funp->env + num_free - 1;
		    while (num_free-- > 1) {
			term = *bptr--;
			ESTACK_PUSH(s, term);
		    }
		    term = *bptr;
		}
	    }
	    break;
	    case REFC_BINARY_SUBTAG:
	    case HEAP_BINARY_SUBTAG:
	    case SUB_BINARY_SUBTAG:
	    {
		byte* bptr;
		unsigned sz = binary_size(term);
		Uint32 con = HCONST_13 + hash;
		Uint bitoffs;
		Uint bitsize;

		ERTS_GET_BINARY_BYTES(term, bptr, bitoffs, bitsize);
		if (sz == 0 && bitsize == 0) {
		    hash = con;
		} else {
		    if (bitoffs == 0) {
			hash = block_hash(bptr, sz, con);
			if (bitsize > 0) {
			    UINT32_HASH_2(bitsize, (bptr[sz] >> (8 - bitsize)),
					  HCONST_15);
			}
		    } else {
			byte* buf = (byte *) erts_alloc(ERTS_ALC_T_TMP,
							sz + (bitsize != 0));
			erts_copy_bits(bptr, bitoffs, 1, buf, 0, 1, sz*8+bitsize);
			hash = block_hash(buf, sz, con);
			if (bitsize > 0) {
			    UINT32_HASH_2(bitsize, (buf[sz] >> (8 - bitsize)),
					  HCONST_15);
			}
			erts_free(ERTS_ALC_T_TMP, (void *) buf);
		    }
		}
		goto hash2_common;
	    }
	    break;
	    case POS_BIG_SUBTAG:
	    case NEG_BIG_SUBTAG:
	    {
		Eterm* ptr = big_val(term);
		Uint i = 0;
		Uint n = BIG_SIZE(ptr);
		Uint32 con = BIG_SIGN(ptr) ? HCONST_10 : HCONST_11;
#if D_EXP == 16
		do {
		    Uint32 x, y;
		    x = i < n ? BIG_DIGIT(ptr, i++) : 0;
		    x += (Uint32)(i < n ? BIG_DIGIT(ptr, i++) : 0) << 16;
		    y = i < n ? BIG_DIGIT(ptr, i++) : 0;
		    y += (Uint32)(i < n ? BIG_DIGIT(ptr, i++) : 0) << 16;
		    UINT32_HASH_2(x, y, con);
		} while (i < n);
#elif D_EXP == 32
		do {
		    Uint32 x, y;
		    x = i < n ? BIG_DIGIT(ptr, i++) : 0;
		    y = i < n ? BIG_DIGIT(ptr, i++) : 0;
		    UINT32_HASH_2(x, y, con);
		} while (i < n);
#elif D_EXP == 64
		do {
		    Uint t;
		    Uint32 x, y;
		    t = i < n ? BIG_DIGIT(ptr, i++) : 0;
		    x = t & 0xffffffff;
		    y = t >> 32;
		    UINT32_HASH_2(x, y, con);
		} while (i < n);
#else
#error "unsupported D_EXP size"
#endif
		goto hash2_common;
	    }
	    break;
	    case REF_SUBTAG:
		/* All parts of the ref should be hashed. */
		UINT32_HASH(internal_ref_numbers(term)[0], HCONST_7);
		goto hash2_common;
		break;
	    case EXTERNAL_REF_SUBTAG:
		/* All parts of the ref should be hashed. */
		UINT32_HASH(external_ref_numbers(term)[0], HCONST_7);
		goto hash2_common;
		break;
	    case EXTERNAL_PID_SUBTAG:
		/* Only 15 bits are hashed. */
		UINT32_HASH(external_pid_number(term), HCONST_5);
		goto hash2_common;
	    case EXTERNAL_PORT_SUBTAG:
		/* Only 15 bits are hashed. */
		UINT32_HASH(external_port_number(term), HCONST_6);
		goto hash2_common;
	    case FLOAT_SUBTAG:
	    {
		FloatDef ff;
		GET_DOUBLE(term, ff);
#if defined(WORDS_BIGENDIAN)
		UINT32_HASH_2(ff.fw[0], ff.fw[1], HCONST_12);
#else
		UINT32_HASH_2(ff.fw[1], ff.fw[0], HCONST_12);
#endif
		goto hash2_common;
	    }
	    break;
		    
	    default:
		erl_exit(1, "Invalid tag in make_hash2(0x%X)\n", term);
	    }
	}
	break;
	case TAG_PRIMARY_IMMED1:
	    switch (term & _TAG_IMMED1_MASK) {
	    case _TAG_IMMED1_PID:
		/* Only 15 bits are hashed. */
		UINT32_HASH(internal_pid_number(term), HCONST_5);
		goto hash2_common;
	    case _TAG_IMMED1_PORT:
		/* Only 15 bits are hashed. */
		UINT32_HASH(internal_port_number(term), HCONST_6);
		goto hash2_common;
	    case _TAG_IMMED1_IMMED2:
		switch (term & _TAG_IMMED2_MASK) {
		case _TAG_IMMED2_ATOM:
		    if (hash == 0)
			/* Fast, but the poor hash value should be mixed. */
			hash = atom_tab(atom_val(term))->slot.bucket.hvalue;
		    else
			UINT32_HASH(atom_tab(atom_val(term))->slot.bucket.hvalue,
				    HCONST_3);
		    goto hash2_common;
		case _TAG_IMMED2_NIL:
		    if (hash == 0)
			hash = 3468870702UL;
		    else
			UINT32_HASH(NIL_DEF, HCONST_2);
		    goto hash2_common;
		default:
		    erl_exit(1, "Invalid tag in make_hash2(0x%X)\n", term);
		}
	    case _TAG_IMMED1_SMALL:
	      {
		  Sint x = signed_val(term);

		  if (SMALL_BITS > 28 && !IS_SSMALL28(x)) {
		      term = small_to_big(x, tmp_big);
		      break;
		  }
		  SINT32_HASH(x, HCONST);
		  goto hash2_common;
	      }
	    }
	    break;
	default:
	    erl_exit(1, "Invalid tag in make_hash2(0x%X)\n", term);
	hash2_common:
	    if (ESTACK_ISEMPTY(s)) {
		DESTROY_ESTACK(s);
		UnUseTmpHeapNoproc(2);
		return hash;
	    }
	    term = ESTACK_POP(s);
	}
    }
    }
#undef UINT32_HASH_2
#undef UINT32_HASH
#undef SINT32_HASH
}

#undef HCONST
#undef MIX


Uint32 make_broken_hash(Eterm term)
{
    Uint32 hash = 0;
    DECLARE_WSTACK(stack);
    unsigned op;
tail_recur:
    op = tag_val_def(term); 
    for (;;) {	
    switch (op) {
    case NIL_DEF:
	hash = hash*FUNNY_NUMBER3 + 1;
	break;
    case ATOM_DEF:
	hash = hash*FUNNY_NUMBER1 +
	    (atom_tab(atom_val(term))->slot.bucket.hvalue);
	break;
    case SMALL_DEF:
#if defined(ARCH_64) && !HALFWORD_HEAP
    {
	Sint y1 = signed_val(term);
	Uint y2 = y1 < 0 ? -(Uint)y1 : y1;
	Uint32 y3 = (Uint32) (y2 >> 32);
	int arity = 1;

#if defined(WORDS_BIGENDIAN)
	if (!IS_SSMALL28(y1))
	{   /* like a bignum */
	    Uint32 y4 = (Uint32) y2;
	    hash = hash*FUNNY_NUMBER2 + ((y4 << 16) | (y4 >> 16));
	    if (y3) 
	    {
		hash = hash*FUNNY_NUMBER2 + ((y3 << 16) | (y3 >> 16));
		arity++;
	    }
	    hash = hash * (y1 < 0 ? FUNNY_NUMBER3 : FUNNY_NUMBER2) + arity;
	} else {
	    hash = hash*FUNNY_NUMBER2 + (((Uint) y1) & 0xfffffff);
	}
#else
	if  (!IS_SSMALL28(y1))
	{   /* like a bignum */
	    hash = hash*FUNNY_NUMBER2 + ((Uint32) y2);
	    if (y3)
	    {
		hash = hash*FUNNY_NUMBER2 + y3;
		arity++;
	    }
	    hash = hash * (y1 < 0 ? FUNNY_NUMBER3 : FUNNY_NUMBER2) + arity;
	} else {
	    hash = hash*FUNNY_NUMBER2 + (((Uint) y1) & 0xfffffff);
	}
#endif
    }
#else
	hash = hash*FUNNY_NUMBER2 + unsigned_val(term);
#endif
	break;

    case BINARY_DEF:
	{
	    size_t sz = binary_size(term);
	    size_t i = (sz < 15) ? sz : 15;

	    hash = hash_binary_bytes(term, i, hash);
	    hash = hash*FUNNY_NUMBER4 + sz;
	    break;
	}

    case EXPORT_DEF:
	{
	    Export* ep = *((Export **) (export_val(term) + 1));

	    hash = hash * FUNNY_NUMBER11 + ep->code[2];
	    hash = hash*FUNNY_NUMBER1 + 
		(atom_tab(atom_val(ep->code[0]))->slot.bucket.hvalue);
	    hash = hash*FUNNY_NUMBER1 + 
		(atom_tab(atom_val(ep->code[1]))->slot.bucket.hvalue);
	    break;
	}

    case FUN_DEF:
	{
	    ErlFunThing* funp = (ErlFunThing *) fun_val(term);
	    Uint num_free = funp->num_free;

	    hash = hash * FUNNY_NUMBER10 + num_free;
	    hash = hash*FUNNY_NUMBER1 + 
		(atom_tab(atom_val(funp->fe->module))->slot.bucket.hvalue);
	    hash = hash*FUNNY_NUMBER2 + funp->fe->old_index;
	    hash = hash*FUNNY_NUMBER2 + funp->fe->old_uniq;
	    if (num_free > 0) {
		if (num_free > 1) {
		    WSTACK_PUSH3(stack, (UWord) &funp->env[1], (num_free-1), MAKE_HASH_FUN_OP);
		}
		term = funp->env[0];
		goto tail_recur;
	    }
	    break;
	}

    case PID_DEF:
	hash = hash*FUNNY_NUMBER5 + internal_pid_number(term);
	break;
    case EXTERNAL_PID_DEF:
	hash = hash*FUNNY_NUMBER5 + external_pid_number(term);
        break;
    case PORT_DEF:
	hash = hash*FUNNY_NUMBER9 + internal_port_number(term);
	break;
    case EXTERNAL_PORT_DEF:
	hash = hash*FUNNY_NUMBER9 + external_port_number(term);
	break;
    case REF_DEF:
	hash = hash*FUNNY_NUMBER9 + internal_ref_numbers(term)[0];
	break;
    case EXTERNAL_REF_DEF:
	hash = hash*FUNNY_NUMBER9 + external_ref_numbers(term)[0];
	break;
    case FLOAT_DEF: 
	{
	    FloatDef ff;
	    GET_DOUBLE(term, ff);
	    hash = hash*FUNNY_NUMBER6 + (ff.fw[0] ^ ff.fw[1]);
	}
	break;

    case MAKE_HASH_CDR_PRE_OP:
	term = (Eterm) WSTACK_POP(stack);
	if (is_not_list(term)) {
	    WSTACK_PUSH(stack, (UWord) MAKE_HASH_CDR_POST_OP);
	    goto tail_recur;
	}
	/*fall through*/
    case LIST_DEF:
	{
	    Eterm* list = list_val(term);
	    WSTACK_PUSH2(stack, (UWord) CDR(list),
			 (UWord) MAKE_HASH_CDR_PRE_OP);
	    term = CAR(list);
	    goto tail_recur;
	}

    case MAKE_HASH_CDR_POST_OP:
	hash *= FUNNY_NUMBER8;
	break;

     case BIG_DEF:
	{
	    Eterm* ptr  = big_val(term);
	    int is_neg = BIG_SIGN(ptr);
	    Uint arity = BIG_ARITY(ptr);
	    Uint i = arity;
	    ptr++;
#if D_EXP == 16
	    /* hash over 32 bit LE */

	    while(i--) {
		hash = hash*FUNNY_NUMBER2 + *ptr++;
	    }
#elif D_EXP == 32

#if defined(WORDS_BIGENDIAN)
	    while(i--) {
		Uint d = *ptr++;
		hash = hash*FUNNY_NUMBER2 + ((d << 16) | (d >> 16));
	    }
#else
	    while(i--) {
		hash = hash*FUNNY_NUMBER2 + *ptr++;
	    }
#endif

#elif D_EXP == 64
	    {
	      Uint32 h = 0, l;
#if defined(WORDS_BIGENDIAN)
	      while(i--) {
		  Uint d = *ptr++;
		  l = d & 0xffffffff;
		  h = d >> 32;
		  hash = hash*FUNNY_NUMBER2 + ((l << 16) | (l >> 16));
		  if (h || i)
		      hash = hash*FUNNY_NUMBER2 + ((h << 16) | (h >> 16));
	      }
#else
	      while(i--) {
		  Uint d = *ptr++;
		  l = d & 0xffffffff;
		  h = d >> 32;
		  hash = hash*FUNNY_NUMBER2 + l;
		  if (h || i)
		      hash = hash*FUNNY_NUMBER2 + h;
	      }
#endif
	      /* adjust arity to match 32 bit mode */
	      arity = (arity << 1) - (h == 0);
	    }

#else
#error "unsupported D_EXP size"	
#endif
	    hash = hash * (is_neg ? FUNNY_NUMBER3 : FUNNY_NUMBER2) + arity;
	}
	break;

    case TUPLE_DEF: 
	{
	    Eterm* ptr = tuple_val(term);
	    Uint arity = arityval(*ptr);

	    WSTACK_PUSH3(stack, (UWord) arity, (UWord) (ptr+1), (UWord) arity);
	    op = MAKE_HASH_TUPLE_OP;
	}/*fall through*/ 
    case MAKE_HASH_TUPLE_OP:
    case MAKE_HASH_FUN_OP:
	{
	    Uint i = (Uint) WSTACK_POP(stack);
	    Eterm* ptr = (Eterm*) WSTACK_POP(stack);
	    if (i != 0) {
		term = *ptr;
		WSTACK_PUSH3(stack, (UWord)(ptr+1), (UWord) i-1, (UWord) op);
		goto tail_recur;
	    }
	    if (op == MAKE_HASH_TUPLE_OP) {
		Uint32 arity = (UWord) WSTACK_POP(stack);
		hash = hash*FUNNY_NUMBER9 + arity;
	    }
	    break;
	}

    default:
	erl_exit(1, "Invalid tag in make_broken_hash\n");
	return 0;
      }
      if (WSTACK_ISEMPTY(stack)) break;
      op = (Uint) WSTACK_POP(stack);
    }

    DESTROY_WSTACK(stack);
    return hash;
    
#undef MAKE_HASH_TUPLE_OP
#undef MAKE_HASH_FUN_OP
#undef MAKE_HASH_CDR_PRE_OP
#undef MAKE_HASH_CDR_POST_OP
}

static int do_send_to_logger(Eterm tag, Eterm gleader, char *buf, int len)
{
    /* error_logger ! 
       {notify,{info_msg,gleader,{emulator,"~s~n",[<message as list>]}}} |
       {notify,{error,gleader,{emulator,"~s~n",[<message as list>]}}} |
       {notify,{warning_msg,gleader,{emulator,"~s~n",[<message as list>}]}} */
    Eterm* hp;
    Uint sz;
    Uint gl_sz;
    Eterm gl;
    Eterm list,plist,format,tuple1,tuple2,tuple3;
    ErlOffHeap *ohp;
    ErlHeapFragment *bp = NULL;
#if !defined(ERTS_SMP)
    Process *p;
#endif

    ASSERT(is_atom(tag));

    if (len <= 0) {
	return -1;
    }

#ifndef ERTS_SMP
    if (
#ifdef USE_THREADS
	!erts_get_scheduler_data() || /* Must be scheduler thread */
#endif
	(p = erts_whereis_process(NULL, 0, am_error_logger, 0, 0)) == NULL
	|| p->status == P_RUNNING) {
	/* buf *always* points to a null terminated string */
	erts_fprintf(stderr, "(no error logger present) %T: \"%s\"\n",
		     tag, buf);
	return 0;
    }
    /* So we have an error logger, lets build the message */
#endif
    gl_sz = IS_CONST(gleader) ? 0 : size_object(gleader);
    sz = len * 2 /* message list */+ 2 /* cons surrounding message list */
	+ gl_sz + 
	3 /*outer 2-tuple*/ + 4 /* middle 3-tuple */ + 4 /*inner 3-tuple */ +
	8 /* "~s~n" */;

#ifndef ERTS_SMP
    if (sz <= HeapWordsLeft(p)) {
	ohp = &MSO(p);
	hp = HEAP_TOP(p);
	HEAP_TOP(p) += sz;
    } else {
#endif
	bp = new_message_buffer(sz);
	ohp = &bp->off_heap;
	hp = bp->mem;
#ifndef ERTS_SMP
    }
#endif
    gl = (is_nil(gleader)
	  ? am_noproc
	  : (IS_CONST(gleader)
	     ? gleader
	     : copy_struct(gleader,gl_sz,&hp,ohp)));
    list = buf_to_intlist(&hp, buf, len, NIL);
    plist = CONS(hp,list,NIL);
    hp += 2;
    format = buf_to_intlist(&hp, "~s~n", 4, NIL);
    tuple1 = TUPLE3(hp, am_emulator, format, plist);
    hp += 4;
    tuple2 = TUPLE3(hp, tag, gl, tuple1);
    hp += 4;
    tuple3 = TUPLE2(hp, am_notify, tuple2);
#ifdef HARDDEBUG
    erts_fprintf(stderr, "%T\n", tuple3);
#endif
#ifdef ERTS_SMP
    {
	Eterm from = erts_get_current_pid();
	if (is_not_internal_pid(from))
	    from = NIL;
	erts_queue_error_logger_message(from, tuple3, bp);
    }
#else
    erts_queue_message(p, NULL /* only used for smp build */, bp, tuple3, NIL);
#endif
    return 0;
}

static ERTS_INLINE int
send_info_to_logger(Eterm gleader, char *buf, int len) 
{
    return do_send_to_logger(am_info_msg, gleader, buf, len);
}

static ERTS_INLINE int
send_warning_to_logger(Eterm gleader, char *buf, int len) 
{
    Eterm tag;
    switch (erts_error_logger_warnings) {
    case am_info:	tag = am_info_msg;	break;
    case am_warning:	tag = am_warning_msg;	break;
    default:		tag = am_error;		break;
    }
    return do_send_to_logger(tag, gleader, buf, len);
}

static ERTS_INLINE int
send_error_to_logger(Eterm gleader, char *buf, int len) 
{
    return do_send_to_logger(am_error, gleader, buf, len);
}

#define LOGGER_DSBUF_INC_SZ 256

static erts_dsprintf_buf_t *
grow_logger_dsbuf(erts_dsprintf_buf_t *dsbufp, size_t need)
{
    size_t size;
    size_t free_size = dsbufp->size - dsbufp->str_len;

    ASSERT(dsbufp && dsbufp->str);

    if (need <= free_size)
	return dsbufp;

    size = need - free_size + LOGGER_DSBUF_INC_SZ;
    size = (((size + LOGGER_DSBUF_INC_SZ - 1) / LOGGER_DSBUF_INC_SZ)
	    * LOGGER_DSBUF_INC_SZ);
    size += dsbufp->size;
    ASSERT(dsbufp->str_len + need <= size);
    dsbufp->str = (char *) erts_realloc(ERTS_ALC_T_LOGGER_DSBUF,
					(void *) dsbufp->str,
					size);
    dsbufp->size = size;
    return dsbufp;
}

erts_dsprintf_buf_t *
erts_create_logger_dsbuf(void)
{
    erts_dsprintf_buf_t init = ERTS_DSPRINTF_BUF_INITER(grow_logger_dsbuf);
    erts_dsprintf_buf_t *dsbufp = erts_alloc(ERTS_ALC_T_LOGGER_DSBUF,
					     sizeof(erts_dsprintf_buf_t));
    sys_memcpy((void *) dsbufp, (void *) &init, sizeof(erts_dsprintf_buf_t));
    dsbufp->str = (char *) erts_alloc(ERTS_ALC_T_LOGGER_DSBUF,
				      LOGGER_DSBUF_INC_SZ);
    dsbufp->str[0] = '\0';
    dsbufp->size = LOGGER_DSBUF_INC_SZ;
    return dsbufp;
}

static ERTS_INLINE void
destroy_logger_dsbuf(erts_dsprintf_buf_t *dsbufp)
{
    ASSERT(dsbufp && dsbufp->str);
    erts_free(ERTS_ALC_T_LOGGER_DSBUF, (void *) dsbufp->str);
    erts_free(ERTS_ALC_T_LOGGER_DSBUF, (void *) dsbufp);
}

int
erts_send_info_to_logger(Eterm gleader, erts_dsprintf_buf_t *dsbufp)
{
    int res;
    res = send_info_to_logger(gleader, dsbufp->str, dsbufp->str_len);
    destroy_logger_dsbuf(dsbufp);
    return res;
}

int
erts_send_warning_to_logger(Eterm gleader, erts_dsprintf_buf_t *dsbufp)
{
    int res;
    res = send_warning_to_logger(gleader, dsbufp->str, dsbufp->str_len);
    destroy_logger_dsbuf(dsbufp);
    return res;
}

int
erts_send_error_to_logger(Eterm gleader, erts_dsprintf_buf_t *dsbufp)
{
    int res;
    res = send_error_to_logger(gleader, dsbufp->str, dsbufp->str_len);
    destroy_logger_dsbuf(dsbufp);
    return res;
}

int
erts_send_info_to_logger_str(Eterm gleader, char *str)
{
    return send_info_to_logger(gleader, str, sys_strlen(str));
}

int
erts_send_warning_to_logger_str(Eterm gleader, char *str)
{
    return send_warning_to_logger(gleader, str, sys_strlen(str));
}

int
erts_send_error_to_logger_str(Eterm gleader, char *str)
{
    return send_error_to_logger(gleader, str, sys_strlen(str));
}

int
erts_send_info_to_logger_nogl(erts_dsprintf_buf_t *dsbuf)
{
    return erts_send_info_to_logger(NIL, dsbuf);
}

int
erts_send_warning_to_logger_nogl(erts_dsprintf_buf_t *dsbuf)
{
    return erts_send_warning_to_logger(NIL, dsbuf);
}

int
erts_send_error_to_logger_nogl(erts_dsprintf_buf_t *dsbuf)
{
    return erts_send_error_to_logger(NIL, dsbuf);
}

int
erts_send_info_to_logger_str_nogl(char *str)
{
    return erts_send_info_to_logger_str(NIL, str);
}

int
erts_send_warning_to_logger_str_nogl(char *str)
{
    return erts_send_warning_to_logger_str(NIL, str);
}

int
erts_send_error_to_logger_str_nogl(char *str)
{
    return erts_send_error_to_logger_str(NIL, str);
}


#define TMP_DSBUF_INC_SZ 256

static erts_dsprintf_buf_t *
grow_tmp_dsbuf(erts_dsprintf_buf_t *dsbufp, size_t need)
{
    size_t size;
    size_t free_size = dsbufp->size - dsbufp->str_len;

    ASSERT(dsbufp);

    if (need <= free_size)
	return dsbufp;
    size = need - free_size + TMP_DSBUF_INC_SZ;
    size = ((size + TMP_DSBUF_INC_SZ - 1)/TMP_DSBUF_INC_SZ)*TMP_DSBUF_INC_SZ;
    size += dsbufp->size;
    ASSERT(dsbufp->str_len + need <= size);
    dsbufp->str = (char *) erts_realloc(ERTS_ALC_T_TMP_DSBUF,
					(void *) dsbufp->str,
					size);
    dsbufp->size = size;
    return dsbufp;
}

erts_dsprintf_buf_t *
erts_create_tmp_dsbuf(Uint size)
{
    Uint init_size = size ? size : TMP_DSBUF_INC_SZ;
    erts_dsprintf_buf_t init = ERTS_DSPRINTF_BUF_INITER(grow_tmp_dsbuf);
    erts_dsprintf_buf_t *dsbufp = erts_alloc(ERTS_ALC_T_TMP_DSBUF,
					     sizeof(erts_dsprintf_buf_t));
    sys_memcpy((void *) dsbufp, (void *) &init, sizeof(erts_dsprintf_buf_t));
    dsbufp->str = (char *) erts_alloc(ERTS_ALC_T_TMP_DSBUF, init_size);
    dsbufp->str[0] = '\0';
    dsbufp->size = init_size;
    return dsbufp;
}

void
erts_destroy_tmp_dsbuf(erts_dsprintf_buf_t *dsbufp)
{
    if (dsbufp->str)
	erts_free(ERTS_ALC_T_TMP_DSBUF, (void *) dsbufp->str);
    erts_free(ERTS_ALC_T_TMP_DSBUF, (void *) dsbufp);
}


/* eq and cmp are written as separate functions a eq is a little faster */

/*
 * Test for equality of two terms.
 * Returns 0 if not equal, or a non-zero value otherwise.
 */

int eq(Eterm a, Eterm b)
{
    DECLARE_WSTACK(stack);
    Sint sz;
    Eterm* aa;
    Eterm* bb;	

tailrecur:
    if (a == b) goto pop_next; 
tailrecur_ne:

    switch (primary_tag(a)) {
    case TAG_PRIMARY_LIST:
	if (is_list(b)) {
	    Eterm* aval = list_val(a);
	    Eterm* bval = list_val(b);
	    while (1) {
		Eterm atmp = CAR(aval);
		Eterm btmp = CAR(bval);
		if (atmp != btmp) {
		    WSTACK_PUSH2(stack,(UWord) CDR(bval),(UWord) CDR(aval));
		    a = atmp;
		    b = btmp;
		    goto tailrecur_ne;
		}
		atmp = CDR(aval);
		btmp = CDR(bval);
		if (atmp == btmp) {
		    goto pop_next;
		}
		if (is_not_list(atmp) || is_not_list(btmp)) {
		    a = atmp;
		    b = btmp;
		    goto tailrecur_ne;
		}
		aval = list_val(atmp);
		bval = list_val(btmp);
	    }
	}
	break; /* not equal */

    case TAG_PRIMARY_BOXED:
	{	
	    Eterm hdr = *boxed_val(a);
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG:
		{
		    aa = tuple_val(a);
		    if (!is_boxed(b) || *boxed_val(b) != *aa)
			goto not_equal;
		    bb = tuple_val(b);
		    if ((sz = arityval(*aa)) == 0) goto pop_next;
		    ++aa;
		    ++bb;
		    goto term_array;
		}
	    case REFC_BINARY_SUBTAG:
	    case HEAP_BINARY_SUBTAG:
	    case SUB_BINARY_SUBTAG:
		{
		    byte* a_ptr;
		    byte* b_ptr;
		    size_t a_size;
		    size_t b_size;
		    Uint a_bitsize;
		    Uint b_bitsize;
		    Uint a_bitoffs;
		    Uint b_bitoffs;
		    
		    if (is_not_binary(b)) {
			goto not_equal;
		    }
		    a_size = binary_size(a);
		    b_size = binary_size(b); 
		    if (a_size != b_size) {
			goto not_equal;
		    }
		    ERTS_GET_BINARY_BYTES(a, a_ptr, a_bitoffs, a_bitsize);
		    ERTS_GET_BINARY_BYTES(b, b_ptr, b_bitoffs, b_bitsize);
		    if ((a_bitsize | b_bitsize | a_bitoffs | b_bitoffs) == 0) {
			if (sys_memcmp(a_ptr, b_ptr, a_size) == 0) goto pop_next;
		    } else if (a_bitsize == b_bitsize) {
			if (erts_cmp_bits(a_ptr, a_bitoffs, b_ptr, b_bitoffs,
					  (a_size << 3) + a_bitsize) == 0) goto pop_next;
		    }
		    break; /* not equal */
		}
	    case EXPORT_SUBTAG:
		{
		    if (is_export(b)) {
			Export* a_exp = *((Export **) (export_val(a) + 1));
			Export* b_exp = *((Export **) (export_val(b) + 1));
			if (a_exp == b_exp) goto pop_next;
		    }
		    break; /* not equal */
		}
	    case FUN_SUBTAG:
		{
		    ErlFunThing* f1;
		    ErlFunThing* f2;
  
		    if (is_not_fun(b))
			goto not_equal;
		    f1 = (ErlFunThing *) fun_val(a);
		    f2 = (ErlFunThing *) fun_val(b);
		    if (f1->fe->module != f2->fe->module ||
			f1->fe->old_index != f2->fe->old_index ||
			f1->fe->old_uniq != f2->fe->old_uniq ||
			f1->num_free != f2->num_free) {
			goto not_equal;
		    }
		    if ((sz = f1->num_free) == 0) goto pop_next;
		    aa = f1->env;
		    bb = f2->env;
		    goto term_array;
		}

	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG: {
		ExternalThing *ap;
		ExternalThing *bp;

		if(is_not_external(b))
		    goto not_equal;

		ap = external_thing_ptr(a);
		bp = external_thing_ptr(b);

		if(ap->header == bp->header && ap->node == bp->node) {
		    ASSERT(1 == external_data_words(a));
		    ASSERT(1 == external_data_words(b));
		    
		    if (ap->data.ui[0] == bp->data.ui[0]) goto pop_next;
		}
		break; /* not equal */
	    }
	    case EXTERNAL_REF_SUBTAG: {
		/*
		 * Observe!
		 *  When comparing refs we need to compare ref numbers
		 * (32-bit words) *not* ref data words.
		 */
		Uint32 *anum;
		Uint32 *bnum;
		Uint common_len;
		Uint alen;
		Uint blen;
		Uint i;

		if(is_not_external_ref(b))
		    goto not_equal;

		if(external_node(a) != external_node(b))
		    goto not_equal;

		anum = external_ref_numbers(a);
		bnum = external_ref_numbers(b);
		alen = external_ref_no_of_numbers(a);
		blen = external_ref_no_of_numbers(b);

		goto ref_common;
	    case REF_SUBTAG:
  
		    if (is_not_internal_ref(b))
			goto not_equal;
		    alen = internal_ref_no_of_numbers(a);
		    blen = internal_ref_no_of_numbers(b);
		    anum = internal_ref_numbers(a);
		    bnum = internal_ref_numbers(b);

	    ref_common:
		    ASSERT(alen > 0 && blen > 0);

		    if (anum[0] != bnum[0])
			goto not_equal;

		    if (alen == 3 && blen == 3) {
			/* Most refs are of length 3 */
			if (anum[1] == bnum[1] && anum[2] == bnum[2]) {
			    goto pop_next; 
			} else {
			    goto not_equal;
			}
		    }

		    common_len = alen;
		    if (blen < alen)
			common_len = blen;

		    for (i = 1; i < common_len; i++)
			if (anum[i] != bnum[i])
			    goto not_equal;

		    if(alen != blen) {

			if (alen > blen) {
			    for (i = common_len; i < alen; i++)
				if (anum[i] != 0)
				    goto not_equal;
			}
			else {
			    for (i = common_len; i < blen; i++)
				if (bnum[i] != 0)
				    goto not_equal;
			}			
		    }
		    goto pop_next;
	    }
	    case POS_BIG_SUBTAG:
	    case NEG_BIG_SUBTAG:
		{
		    int i;
  
		    if (is_not_big(b))
			goto not_equal;
		    aa = big_val(a); /* get pointer to thing */
		    bb = big_val(b);
		    if (*aa != *bb)
			goto not_equal;
		    i = BIG_ARITY(aa);
		    while(i--) {
			if (*++aa != *++bb)
			    goto not_equal;
		    }
		    goto pop_next;
		}
	    case FLOAT_SUBTAG:
		{
		    FloatDef af;
		    FloatDef bf;
  
		    if (is_float(b)) {
			GET_DOUBLE(a, af);
			GET_DOUBLE(b, bf);
			if (af.fd == bf.fd) goto pop_next;
		    }
		    break; /* not equal */
		}
	    }
	    break;
	}
    }
    goto not_equal;


term_array: /* arrays in 'aa' and 'bb', length in 'sz' */
    ASSERT(sz != 0);
    {
	Eterm* ap = aa;
	Eterm* bp = bb;
	Sint i = sz;
	for (;;) {
	    if (*ap != *bp) break;
	    if (--i == 0) goto pop_next;
	    ++ap;
	    ++bp;
	}
	a = *ap;
	b = *bp;
	if (is_both_immed(a,b)) {
	    goto not_equal;
	}
	if (i > 1) { /* push the rest */
	    WSTACK_PUSH3(stack, i-1, (UWord)(bp+1),
			 ((UWord)(ap+1)) | TAG_PRIMARY_HEADER);
	    /* We (ab)use TAG_PRIMARY_HEADER to recognize a term_array */
	}
	goto tailrecur_ne;
    }
   
pop_next:
    if (!WSTACK_ISEMPTY(stack)) {
	UWord something  = WSTACK_POP(stack);
	if (primary_tag((Eterm) something) == TAG_PRIMARY_HEADER) { /* a term_array */
	    aa = (Eterm*) something;
	    bb = (Eterm*) WSTACK_POP(stack);
	    sz = WSTACK_POP(stack);
	    goto term_array;
	}
	a = something;
	b = WSTACK_POP(stack);
	goto tailrecur;
    }

    DESTROY_WSTACK(stack);
    return 1;

not_equal:
    DESTROY_WSTACK(stack);
    return 0;
}


/* 
 * Lexically compare two strings of bytes (string s1 length l1 and s2 l2).
 *
 *	s1 < s2	return -1
 *	s1 = s2	return  0
 *	s1 > s2 return +1
 */
static int cmpbytes(byte *s1, int l1, byte *s2, int l2)
{
    int i;
    i = 0;
    while((i < l1) && (i < l2)) {
	if (s1[i] < s2[i]) return(-1);
	if (s1[i] > s2[i]) return(1);
	i++;
    }
    if (l1 < l2) return(-1);
    if (l1 > l2) return(1);
    return(0);
}


/*
 * Compare objects.
 * Returns 0 if equal, a negative value if a < b, or a positive number a > b.
 *
 * According to the Erlang Standard, types are orderered as follows:
 *   numbers < (characters) < atoms < refs < funs < ports < pids <
 *   tuples < [] < conses < binaries.
 *
 * Note that characters are currently not implemented.
 *
 */


#define float_comp(x,y)    (((x)<(y)) ? -1 : (((x)==(y)) ? 0 : 1))

static int cmp_atoms(Eterm a, Eterm b)
{
    Atom *aa = atom_tab(atom_val(a));
    Atom *bb = atom_tab(atom_val(b));
    int diff = aa->ord0 - bb->ord0;
    if (diff)
	return diff;
    return cmpbytes(aa->name+3, aa->len-3,
		    bb->name+3, bb->len-3);
}

Sint cmp(Eterm a, Eterm b)
{
    DECLARE_WSTACK(stack);
    Eterm* aa;
    Eterm* bb;
    int i;
    Sint j;
    int a_tag;
    int b_tag;
    ErlNode *anode;
    ErlNode *bnode;
    Uint adata;
    Uint bdata;
    Uint alen;
    Uint blen;
    Uint32 *anum;
    Uint32 *bnum;

#define RETURN_NEQ(cmp) { j=(cmp); ASSERT(j != 0); goto not_equal; }
#define ON_CMP_GOTO(cmp) if ((j=(cmp)) == 0) goto pop_next; else goto not_equal

#undef  CMP_NODES
#define CMP_NODES(AN, BN)						\
    do {								\
	if((AN) != (BN)) {						\
            if((AN)->sysname != (BN)->sysname)				\
                RETURN_NEQ(cmp_atoms((AN)->sysname, (BN)->sysname));	\
	    ASSERT((AN)->creation != (BN)->creation);			\
	    RETURN_NEQ(((AN)->creation < (BN)->creation) ? -1 : 1);	\
	}								\
    } while (0)


tailrecur:
    if (a == b) {		/* Equal values or pointers. */
	goto pop_next;
    }
tailrecur_ne:

    /* deal with majority (?) cases by brute-force */
    if (is_atom(a)) {
	if (is_atom(b)) {
	    ON_CMP_GOTO(cmp_atoms(a, b));
	}
    } else if (is_both_small(a, b)) {
	ON_CMP_GOTO(signed_val(a) - signed_val(b));
    }

    /*
     * Take care of cases where the types are the same.
     */

    a_tag = 42;			/* Suppress warning */
    switch (primary_tag(a)) {
    case TAG_PRIMARY_IMMED1:
	switch ((a & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
	case (_TAG_IMMED1_PORT >> _TAG_PRIMARY_SIZE):
	    if (is_internal_port(b)) {
		bnode = erts_this_node;
		bdata = internal_port_data(b);
	    } else if (is_external_port(b)) {
		bnode = external_port_node(b);
		bdata = external_port_data(b);
	    } else {
		a_tag = PORT_DEF;
		goto mixed_types;
	    }
	    anode = erts_this_node;
	    adata = internal_port_data(a);
		
	port_common:
	    CMP_NODES(anode, bnode);
	    ON_CMP_GOTO((Sint)(adata - bdata));

	case (_TAG_IMMED1_PID >> _TAG_PRIMARY_SIZE):
	    if (is_internal_pid(b)) {
		bnode = erts_this_node;
		bdata = internal_pid_data(b);
	    } else if (is_external_pid(b)) {
		bnode = external_pid_node(b);
		bdata = external_pid_data(b);
	    } else {
		a_tag = PID_DEF;
		goto mixed_types;
	    }
	    anode = erts_this_node;
	    adata = internal_pid_data(a);
	    
	pid_common:
	    if (adata != bdata) {
		RETURN_NEQ(adata < bdata ? -1 : 1);
	    }
	    CMP_NODES(anode, bnode);
	    goto pop_next;
	case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
	    a_tag = SMALL_DEF;
	    goto mixed_types;
	case (_TAG_IMMED1_IMMED2 >> _TAG_PRIMARY_SIZE): {
	    switch ((a & _TAG_IMMED2_MASK) >> _TAG_IMMED1_SIZE) {
	    case (_TAG_IMMED2_ATOM >> _TAG_IMMED1_SIZE):
		a_tag = ATOM_DEF;
		goto mixed_types;
	    case (_TAG_IMMED2_NIL >> _TAG_IMMED1_SIZE):
		a_tag = NIL_DEF;
		goto mixed_types;
	    }
	}
	}
    case TAG_PRIMARY_LIST:
	if (is_not_list(b)) {
	    a_tag = LIST_DEF;
	    goto mixed_types;
	}
	aa = list_val(a);
	bb = list_val(b);
	while (1) {
	    Eterm atmp = CAR(aa);
	    Eterm btmp = CAR(bb);
	    if (atmp != btmp) {
		WSTACK_PUSH2(stack,(UWord) CDR(bb),(UWord) CDR(aa));
		a = atmp;
		b = btmp;
		goto tailrecur_ne;
	    }
	    atmp = CDR(aa);
	    btmp = CDR(bb);
	    if (atmp == btmp) {
		goto pop_next;
	    }
	    if (is_not_list(atmp) || is_not_list(btmp)) {
		a = atmp;
		b = btmp;
		goto tailrecur_ne;
	    }
	    aa = list_val(atmp);
	    bb = list_val(btmp);
	}
    case TAG_PRIMARY_BOXED:
	{
	    Eterm ahdr = *boxed_val(a);
	    switch ((ahdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
	    case (_TAG_HEADER_ARITYVAL >> _TAG_PRIMARY_SIZE):
		if (is_not_tuple(b)) {
		    a_tag = TUPLE_DEF;
		    goto mixed_types;
		}
		aa = tuple_val(a);
		bb = tuple_val(b);
		/* compare the arities */
		i = arityval(ahdr);	/* get the arity*/
		if (i != arityval(*bb)) {
		    RETURN_NEQ((int)(i - arityval(*bb)));
		}
		if (i == 0) {
		    goto pop_next;
		}
		++aa;
		++bb;
		goto term_array;

	    case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		if (is_not_float(b)) {
		    a_tag = FLOAT_DEF;
		    goto mixed_types;
		} else {
		    FloatDef af;
		    FloatDef bf; 

		    GET_DOUBLE(a, af);
		    GET_DOUBLE(b, bf);
		    ON_CMP_GOTO(float_comp(af.fd, bf.fd));
		}
	    case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
	    case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		if (is_not_big(b)) {
		    a_tag = BIG_DEF;
		    goto mixed_types;
		}
		ON_CMP_GOTO(big_comp(a, b));
	    case (_TAG_HEADER_EXPORT >> _TAG_PRIMARY_SIZE):
		if (is_not_export(b)) {
		    a_tag = EXPORT_DEF;
		    goto mixed_types;
		} else {
		    Export* a_exp = *((Export **) (export_val(a) + 1));
		    Export* b_exp = *((Export **) (export_val(b) + 1));

		    if ((j = cmp_atoms(a_exp->code[0], b_exp->code[0])) != 0) {
			RETURN_NEQ(j);
		    }
		    if ((j = cmp_atoms(a_exp->code[1], b_exp->code[1])) != 0) {
			RETURN_NEQ(j);
		    }
		    ON_CMP_GOTO((Sint) a_exp->code[2] - (Sint) b_exp->code[2]);
		}
		break;
	    case (_TAG_HEADER_FUN >> _TAG_PRIMARY_SIZE):
		if (is_not_fun(b)) {
		    a_tag = FUN_DEF;
		    goto mixed_types;
		} else {
		    ErlFunThing* f1 = (ErlFunThing *) fun_val(a);
		    ErlFunThing* f2 = (ErlFunThing *) fun_val(b);
		    Sint diff;

		    diff = cmpbytes(atom_tab(atom_val(f1->fe->module))->name,
				    atom_tab(atom_val(f1->fe->module))->len,
				    atom_tab(atom_val(f2->fe->module))->name,
				    atom_tab(atom_val(f2->fe->module))->len);
		    if (diff != 0) {
			RETURN_NEQ(diff);
		    }
		    diff = f1->fe->old_index - f2->fe->old_index;
		    if (diff != 0) {
			RETURN_NEQ(diff);
		    }
		    diff = f1->fe->old_uniq - f2->fe->old_uniq;
		    if (diff != 0) {
			RETURN_NEQ(diff);
		    }
		    diff = f1->num_free - f2->num_free;
		    if (diff != 0) {
			RETURN_NEQ(diff);
		    }		
		    i = f1->num_free;
		    if (i == 0) goto pop_next;
		    aa = f1->env;
		    bb = f2->env;
		    goto term_array;
		}
	    case (_TAG_HEADER_EXTERNAL_PID >> _TAG_PRIMARY_SIZE):
		if (is_internal_pid(b)) {
		    bnode = erts_this_node;
		    bdata = internal_pid_data(b);
		} else if (is_external_pid(b)) {
		    bnode = external_pid_node(b);
		    bdata = external_pid_data(b);
		} else {
		    a_tag = EXTERNAL_PID_DEF;
		    goto mixed_types;
		}
		anode = external_pid_node(a);
		adata = external_pid_data(a);
		goto pid_common;
	    case (_TAG_HEADER_EXTERNAL_PORT >> _TAG_PRIMARY_SIZE):
		if (is_internal_port(b)) {
		    bnode = erts_this_node;
		    bdata = internal_port_data(b);
		} else if (is_external_port(b)) {
		    bnode = external_port_node(b);
		    bdata = external_port_data(b);
		} else {
		    a_tag = EXTERNAL_PORT_DEF;
		    goto mixed_types;
		}
		anode = external_port_node(a);
		adata = external_port_data(a);
		goto port_common;
	    case (_TAG_HEADER_REF >> _TAG_PRIMARY_SIZE):
		/*
		 * Note! When comparing refs we need to compare ref numbers
		 * (32-bit words), *not* ref data words.
		 */
		
		if (is_internal_ref(b)) {
		    bnode = erts_this_node;
		    bnum = internal_ref_numbers(b);
		    blen = internal_ref_no_of_numbers(b);
		} else if(is_external_ref(b)) {
		    bnode = external_ref_node(b);
		    bnum = external_ref_numbers(b);
		    blen = external_ref_no_of_numbers(b);
		} else {
		    a_tag = REF_DEF;
		    goto mixed_types;
		}
		anode = erts_this_node;
		anum = internal_ref_numbers(a);
		alen = internal_ref_no_of_numbers(a);
		
	    ref_common:
		CMP_NODES(anode, bnode);
		
		ASSERT(alen > 0 && blen > 0);
		if (alen != blen) {
		    if (alen > blen) {
			do {
			    if (anum[alen - 1] != 0)
				RETURN_NEQ(1);
			    alen--;
			} while (alen > blen);
		    }
		    else {
			do {
			    if (bnum[blen - 1] != 0)
				RETURN_NEQ(-1);
			    blen--;
			} while (alen < blen);
		    }
		}
		
		ASSERT(alen == blen);
		for (i = (Sint) alen - 1; i >= 0; i--)
		    if (anum[i] != bnum[i])
			RETURN_NEQ((Sint32) (anum[i] - bnum[i]));
		goto pop_next;
	    case (_TAG_HEADER_EXTERNAL_REF >> _TAG_PRIMARY_SIZE):
		if (is_internal_ref(b)) {
		    bnode = erts_this_node;
		    bnum = internal_ref_numbers(b);
		    blen = internal_ref_no_of_numbers(b);
		} else if (is_external_ref(b)) {
		    bnode = external_ref_node(b);
		    bnum = external_ref_numbers(b);
		    blen = external_ref_no_of_numbers(b);
		} else {
		    a_tag = EXTERNAL_REF_DEF;
		    goto mixed_types;
		}
		anode = external_ref_node(a);
		anum = external_ref_numbers(a);
		alen = external_ref_no_of_numbers(a);
		goto ref_common;
	    default:
		/* Must be a binary */
		ASSERT(is_binary(a));
		if (is_not_binary(b)) {
		    a_tag = BINARY_DEF;
		    goto mixed_types;
		} else {
		    Uint a_size = binary_size(a);
		    Uint b_size = binary_size(b);
		    Uint a_bitsize;
		    Uint b_bitsize;
		    Uint a_bitoffs;
		    Uint b_bitoffs;
		    Uint min_size;
		    int cmp;
		    byte* a_ptr;
		    byte* b_ptr;
		    ERTS_GET_BINARY_BYTES(a, a_ptr, a_bitoffs, a_bitsize);
		    ERTS_GET_BINARY_BYTES(b, b_ptr, b_bitoffs, b_bitsize);
		    if ((a_bitsize | b_bitsize | a_bitoffs | b_bitoffs) == 0) {
			min_size = (a_size < b_size) ? a_size : b_size;
			if ((cmp = sys_memcmp(a_ptr, b_ptr, min_size)) != 0) {
			    RETURN_NEQ(cmp);
			}
		    }
		    else {
			a_size = (a_size << 3) + a_bitsize;
			b_size = (b_size << 3) + b_bitsize;
			min_size = (a_size < b_size) ? a_size : b_size;
			if ((cmp = erts_cmp_bits(a_ptr,a_bitoffs,
						 b_ptr,b_bitoffs,min_size)) != 0) {
			    RETURN_NEQ(cmp);
			}
		    }
		    ON_CMP_GOTO((Sint)(a_size - b_size));
		}
	    }
	}
    }

    /*
     * Take care of the case that the tags are different.
     */

 mixed_types:
    b_tag = tag_val_def(b);

    {
	FloatDef f1, f2;
	Eterm big;
#if HEAP_ON_C_STACK
	Eterm big_buf[2]; /* If HEAP_ON_C_STACK */
#else
	Eterm *big_buf = erts_get_scheduler_data()->cmp_tmp_heap;
#endif

	switch(_NUMBER_CODE(a_tag, b_tag)) {
	case SMALL_BIG:
	    big = small_to_big(signed_val(a), big_buf);
	    j = big_comp(big, b);
	    break;
	case SMALL_FLOAT:
	    f1.fd = signed_val(a);
	    GET_DOUBLE(b, f2);
	    j = float_comp(f1.fd, f2.fd);
	    break;
	case BIG_SMALL:
	    big = small_to_big(signed_val(b), big_buf);
	    j = big_comp(a, big);
	    break;
	case BIG_FLOAT:
	    if (big_to_double(a, &f1.fd) < 0) {
		j = big_sign(a) ? -1 : 1;
	    } else {
		GET_DOUBLE(b, f2);
		j = float_comp(f1.fd, f2.fd);
	    }
	    break;
	case FLOAT_SMALL:
	    GET_DOUBLE(a, f1);
	    f2.fd = signed_val(b);
	    j = float_comp(f1.fd, f2.fd);
	    break;
	case FLOAT_BIG:
	    if (big_to_double(b, &f2.fd) < 0) {
		j = big_sign(b) ? 1 : -1;
	    } else {
		GET_DOUBLE(a, f1);
		j = float_comp(f1.fd, f2.fd);
	    }
	    break;
	default:
	    j = b_tag - a_tag;
	}
    }
    if (j == 0) {
	goto pop_next; 
    } else {
	goto not_equal;
    }

term_array: /* arrays in 'aa' and 'bb', length in 'i' */
    ASSERT(i>0);
    while (--i) {
	a = *aa++;
	b = *bb++;
	if (a != b) {
	    if (is_atom(a) && is_atom(b)) {
		if ((j = cmp_atoms(a, b)) != 0) {
		    goto not_equal;
		}
	    } else if (is_both_small(a, b)) {
		if ((j = signed_val(a)-signed_val(b)) != 0) {
		    goto not_equal;
		}
	    } else {
		/* (ab)Use TAG_PRIMARY_HEADER to recognize a term_array */
		WSTACK_PUSH3(stack, i, (UWord)bb, (UWord)aa | TAG_PRIMARY_HEADER);
		goto tailrecur_ne;
	    }
	}
    }
    a = *aa;
    b = *bb;
    goto tailrecur;    
   
pop_next:
    if (!WSTACK_ISEMPTY(stack)) {
	UWord something = WSTACK_POP(stack);
	if (primary_tag((Eterm) something) == TAG_PRIMARY_HEADER) { /* a term_array */
	    aa = (Eterm*) something;
	    bb = (Eterm*) WSTACK_POP(stack);
	    i = WSTACK_POP(stack);
	    goto term_array;
	}
	a = (Eterm) something;
	b = (Eterm) WSTACK_POP(stack);
	goto tailrecur;
    }

    DESTROY_WSTACK(stack);
    return 0;

not_equal:
    DESTROY_ESTACK(stack);
    return j;

#undef CMP_NODES
}


void
erts_cleanup_externals(ExternalThing *etp)
{
    ExternalThing *tetp;

    tetp = etp;

    while(tetp) {
	erts_deref_node_entry(tetp->node);
	tetp = tetp->next;
    }
}

Eterm
store_external_or_ref_(Uint **hpp, ExternalThing **etpp, Eterm ns)
{
    Uint i;
    Uint size;
    Uint *from_hp;
    Uint *to_hp = *hpp;

    ASSERT(is_external(ns) || is_internal_ref(ns));

    if(is_external(ns)) {
	from_hp = external_val(ns);
	size = thing_arityval(*from_hp) + 1;
	*hpp += size;

	for(i = 0; i < size; i++)
	    to_hp[i] = from_hp[i];

	erts_refc_inc(&((ExternalThing *) to_hp)->node->refc, 2);

	((ExternalThing *) to_hp)->next = *etpp;
	*etpp = (ExternalThing *) to_hp;

	return make_external(to_hp);
    }

    /* Internal ref */
    from_hp = internal_ref_val(ns);

    size = thing_arityval(*from_hp) + 1;

    *hpp += size;

    for(i = 0; i < size; i++)
	to_hp[i] = from_hp[i];

    return make_internal_ref(to_hp);
}

Eterm
store_external_or_ref_in_proc_(Process *proc, Eterm ns)
{
    Uint sz;
    Uint *hp;

    ASSERT(is_external(ns) || is_internal_ref(ns));

    sz = NC_HEAP_SIZE(ns);
    ASSERT(sz > 0);
    hp = HAlloc(proc, sz);
    return store_external_or_ref_(&hp, &MSO(proc).externals, ns);
}

void bin_write(int to, void *to_arg, byte* buf, int sz)
{
    int i;

    for (i=0;i<sz;i++) {
	if (IS_DIGIT(buf[i]))
	    erts_print(to, to_arg, "%d,", buf[i]);
	else if (IS_PRINT(buf[i])) {
	    erts_print(to, to_arg, "%c,", buf[i]);
	}
	else
	    erts_print(to, to_arg, "%d,", buf[i]);
    }
    erts_putc(to, to_arg, '\n');
}

/* Fill buf with the contents of bytelist list 
   return number of chars in list or -1 for error */

int
intlist_to_buf(Eterm list, char *buf, int len)
{
    Eterm* listptr;
    int sz = 0;

    if (is_nil(list)) 
	return 0;
    if (is_not_list(list))
	return -1;
    listptr = list_val(list);

    while (sz < len) {
	if (!is_byte(*listptr)) 
	    return -1;
	buf[sz++] = unsigned_val(*listptr);
	if (is_nil(*(listptr + 1)))
	    return(sz);
	if (is_not_list(*(listptr + 1))) 
	    return -1;
	listptr = list_val(*(listptr + 1));
    }
    return -1;			/* not enough space */
}

/*
** Convert an integer to a byte list
** return pointer to converted stuff (need not to be at start of buf!)
*/
char* Sint_to_buf(Sint n, struct Sint_buf *buf)
{
    char* p = &buf->s[sizeof(buf->s)-1];
    int sign = 0;

    *p-- = '\0'; /* null terminate */
    if (n == 0)
	*p-- = '0';
    else if (n < 0) {
	sign = 1;
	n = -n;
    }

    while (n != 0) {
	*p-- = (n % 10) + '0';
	n /= 10;
    }
    if (sign)
	*p-- = '-';
    return p+1;
}

/* Build a list of integers in some safe memory area
** Memory must be pre allocated prio call 2*len in size
** hp is a pointer to the "heap" pointer on return
** this pointer is updated to point after the list
*/

Eterm
buf_to_intlist(Eterm** hpp, char *buf, int len, Eterm tail)
{
    Eterm* hp = *hpp;

    buf += (len-1);
    while(len > 0) {
	tail = CONS(hp, make_small((byte)*buf), tail);
	hp += 2;
	buf--;
	len--;
    }
    *hpp = hp;
    return tail;
}

/*
** Write io list in to a buffer.
**
** An iolist is defined as:
**
** iohead ::= Binary
**        |   Byte (i.e integer in range [0..255]
**        |   iolist
**        ;
**
** iotail ::= []
**        |   Binary  (added by tony)
**        |   iolist
**        ;
**
** iolist ::= []
**        |   Binary
**        |   [ iohead | iotail]
**        ;
** 
** Return remaining bytes in buffer on success
**        -1 on overflow
**        -2 on type error (including that result would not be a whole number of bytes)
*/

int io_list_to_buf(Eterm obj, char* buf, int len)
{
    Eterm* objp;
    DECLARE_ESTACK(s);
    goto L_again;
    
    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_again:
	if (is_list(obj)) {
	L_iter_list:
	    objp = list_val(obj);
	    obj = CAR(objp);
	    if (is_byte(obj)) {
		if (len == 0) {
		    goto L_overflow;
		}
		*buf++ = unsigned_val(obj);
		len--;
	    } else if (is_binary(obj)) {
		byte* bptr;
		size_t size = binary_size(obj);
		Uint bitsize;
		Uint bitoffs;
		Uint num_bits;
		
		if (len < size) {
		    goto L_overflow;
		}
		ERTS_GET_BINARY_BYTES(obj, bptr, bitoffs, bitsize);
		if (bitsize != 0) {
		    goto L_type_error;
		}
		num_bits = 8*size;
		copy_binary_to_buffer(buf, 0, bptr, bitoffs, num_bits);
		buf += size;
		len -= size;
	    } else if (is_list(obj)) {
		ESTACK_PUSH(s, CDR(objp));
		goto L_iter_list; /* on head */
	    } else if (is_not_nil(obj)) {
		goto L_type_error;
	    }

	    obj = CDR(objp);
	    if (is_list(obj)) {
		goto L_iter_list; /* on tail */
	    } else if (is_binary(obj)) {
		byte* bptr;
		size_t size = binary_size(obj);
		Uint bitsize;
		Uint bitoffs;
		Uint num_bits;
		if (len < size) {
		    goto L_overflow;
		}
		ERTS_GET_BINARY_BYTES(obj, bptr, bitoffs, bitsize);
		if (bitsize != 0) {
		    goto L_type_error;
		}
		num_bits = 8*size;
		copy_binary_to_buffer(buf, 0, bptr, bitoffs, num_bits);
		buf += size;
		len -= size;
	    } else if (is_not_nil(obj)) {
		goto L_type_error;
	    }
	} else if (is_binary(obj)) {
	    byte* bptr;
	    size_t size = binary_size(obj);
	    Uint bitsize;
	    Uint bitoffs;
	    Uint num_bits;
	    if (len < size) {
		goto L_overflow;
	    }
	    ERTS_GET_BINARY_BYTES(obj, bptr, bitoffs, bitsize);
	    if (bitsize != 0) {
		goto L_type_error;
	    }
	    num_bits = 8*size;
	    copy_binary_to_buffer(buf, 0, bptr, bitoffs, num_bits);
	    buf += size;
	    len -= size;
	} else if (is_not_nil(obj)) {
	    goto L_type_error;
	}
    }
      
    DESTROY_ESTACK(s);
    return len;

 L_type_error:
    DESTROY_ESTACK(s);
    return -2;

 L_overflow:
    DESTROY_ESTACK(s);
    return -1;
}

int io_list_len(Eterm obj)
{
    Eterm* objp;
    Sint len = 0;
    DECLARE_ESTACK(s);
    goto L_again;

    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_again:
	if (is_list(obj)) {
	L_iter_list:
	    objp = list_val(obj);
	    /* Head */
	    obj = CAR(objp);
	    if (is_byte(obj)) {
		len++;
	    } else if (is_binary(obj) && binary_bitsize(obj) == 0) {
		len += binary_size(obj);
	    } else if (is_list(obj)) {
		ESTACK_PUSH(s, CDR(objp));
		goto L_iter_list; /* on head */
	    } else if (is_not_nil(obj)) {
		goto L_type_error;
	    }
	    /* Tail */
	    obj = CDR(objp);
	    if (is_list(obj))
		goto L_iter_list; /* on tail */
	    else if (is_binary(obj) && binary_bitsize(obj) == 0) {
		len += binary_size(obj);
	    } else if (is_not_nil(obj)) {
		goto L_type_error;
	    }
	} else if (is_binary(obj) && binary_bitsize(obj) == 0) { /* Tail was binary */
	    len += binary_size(obj);
	} else if (is_not_nil(obj)) {
	    goto L_type_error;
	}
    }

    DESTROY_ESTACK(s);
    return len;

 L_type_error:
    DESTROY_ESTACK(s);
    return -1;
}

/* return 0 if item is not a non-empty flat list of bytes */
int
is_string(Eterm list)
{
    int len = 0;

    while(is_list(list)) {
	Eterm* consp = list_val(list);
	Eterm hd = CAR(consp);

	if (!is_byte(hd))
	    return 0;
	len++;
	list = CDR(consp);
    }
    if (is_nil(list))
	return len;
    return 0;
}

#ifdef ERTS_SMP

/*
 * Process and Port timers in smp case
 */

ERTS_SCHED_PREF_PRE_ALLOC_IMPL(ptimer_pre, ErtsSmpPTimer, 1000)

#define ERTS_PTMR_FLGS_ALLCD_SIZE \
  2
#define ERTS_PTMR_FLGS_ALLCD_MASK \
  ((((Uint32) 1) << ERTS_PTMR_FLGS_ALLCD_SIZE) - 1)

#define ERTS_PTMR_FLGS_PREALLCD	((Uint32) 1)
#define ERTS_PTMR_FLGS_SLALLCD	((Uint32) 2)
#define ERTS_PTMR_FLGS_LLALLCD	((Uint32) 3)
#define ERTS_PTMR_FLG_CANCELLED	(((Uint32) 1) << (ERTS_PTMR_FLGS_ALLCD_SIZE+0))

static void
init_ptimers(void)
{
    init_ptimer_pre_alloc();
}

static ERTS_INLINE void
free_ptimer(ErtsSmpPTimer *ptimer)
{
    switch (ptimer->timer.flags & ERTS_PTMR_FLGS_ALLCD_MASK) {
    case ERTS_PTMR_FLGS_PREALLCD:
	(void) ptimer_pre_free(ptimer);
	break;
    case ERTS_PTMR_FLGS_SLALLCD:
	erts_free(ERTS_ALC_T_SL_PTIMER, (void *) ptimer);
	break;
    case ERTS_PTMR_FLGS_LLALLCD:
	erts_free(ERTS_ALC_T_LL_PTIMER, (void *) ptimer);
	break;
    default:
	erl_exit(ERTS_ABORT_EXIT,
		 "Internal error: Bad ptimer alloc type\n");
	break;
    }
}

/* Callback for process timeout cancelled */
static void
ptimer_cancelled(ErtsSmpPTimer *ptimer)
{
    free_ptimer(ptimer);
}

/* Callback for process timeout */
static void
ptimer_timeout(ErtsSmpPTimer *ptimer)
{
    if (is_internal_pid(ptimer->timer.id)) {
	Process *p;
	p = erts_pid2proc_opt(NULL,
			      0,
			      ptimer->timer.id,
			      ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS,
			      ERTS_P2P_FLG_ALLOW_OTHER_X);
	if (p) {
	    if (!p->is_exiting
		&& !(ptimer->timer.flags & ERTS_PTMR_FLG_CANCELLED)) {
		ASSERT(*ptimer->timer.timer_ref == ptimer);
		*ptimer->timer.timer_ref = NULL;
		(*ptimer->timer.timeout_func)(p);
	    }
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS);
	}
    }
    else {
	Port *p;
	ASSERT(is_internal_port(ptimer->timer.id));
	p = erts_id2port_sflgs(ptimer->timer.id,
			       NULL,
			       0,
			       ERTS_PORT_SFLGS_DEAD);
	if (p) {
	    if (!(ptimer->timer.flags & ERTS_PTMR_FLG_CANCELLED)) {
		ASSERT(*ptimer->timer.timer_ref == ptimer);
		*ptimer->timer.timer_ref = NULL;
		(*ptimer->timer.timeout_func)(p);
	    }
	    erts_port_release(p);
	}
    }
    free_ptimer(ptimer);
}

void
erts_create_smp_ptimer(ErtsSmpPTimer **timer_ref,
		       Eterm id,
		       ErlTimeoutProc timeout_func,
		       Uint timeout)
{
    ErtsSmpPTimer *res = ptimer_pre_alloc();
    if (res)
	res->timer.flags = ERTS_PTMR_FLGS_PREALLCD;
    else {
	if (timeout < ERTS_ALC_MIN_LONG_LIVED_TIME) {
	    res = erts_alloc(ERTS_ALC_T_SL_PTIMER, sizeof(ErtsSmpPTimer));
	    res->timer.flags = ERTS_PTMR_FLGS_SLALLCD;
	}
	else {
	    res = erts_alloc(ERTS_ALC_T_LL_PTIMER, sizeof(ErtsSmpPTimer));
	    res->timer.flags = ERTS_PTMR_FLGS_LLALLCD;
	}
    }
    res->timer.timeout_func = timeout_func;
    res->timer.timer_ref = timer_ref;
    res->timer.id = id;
    res->timer.tm.active = 0; /* MUST be initalized */

    ASSERT(!*timer_ref);

    *timer_ref = res;

    erl_set_timer(&res->timer.tm,
		  (ErlTimeoutProc) ptimer_timeout,
		  (ErlCancelProc) ptimer_cancelled,
		  (void*) res,
		  timeout);
}

void
erts_cancel_smp_ptimer(ErtsSmpPTimer *ptimer)
{
    if (ptimer) {
	ASSERT(*ptimer->timer.timer_ref == ptimer);
	*ptimer->timer.timer_ref = NULL;
	ptimer->timer.flags |= ERTS_PTMR_FLG_CANCELLED;
	erl_cancel_timer(&ptimer->timer.tm);
    }
}

#endif

static Sint trim_threshold;
static Sint top_pad;
static Sint mmap_threshold;
static Sint mmap_max;

Uint tot_bin_allocated;

void erts_init_utils(void)
{
#ifdef ERTS_SMP
    init_ptimers();
#endif
}

void erts_init_utils_mem(void) 
{
    trim_threshold = -1;
    top_pad = -1;
    mmap_threshold = -1;
    mmap_max = -1;
}

int
sys_alloc_opt(int opt, int value)
{
#if HAVE_MALLOPT
  Sint m_opt;
  Sint *curr_val;

  switch(opt) {
  case SYS_ALLOC_OPT_TRIM_THRESHOLD:
#ifdef M_TRIM_THRESHOLD
    m_opt = M_TRIM_THRESHOLD;
    curr_val = &trim_threshold;
    break;
#else
    return 0;
#endif
  case SYS_ALLOC_OPT_TOP_PAD:
#ifdef M_TOP_PAD
    m_opt = M_TOP_PAD;
    curr_val = &top_pad;
    break;
#else
    return 0;
#endif
  case SYS_ALLOC_OPT_MMAP_THRESHOLD:
#ifdef M_MMAP_THRESHOLD
    m_opt = M_MMAP_THRESHOLD;
    curr_val = &mmap_threshold;
    break;
#else
    return 0;
#endif
  case SYS_ALLOC_OPT_MMAP_MAX:
#ifdef M_MMAP_MAX
    m_opt = M_MMAP_MAX;
    curr_val = &mmap_max;
    break;
#else
    return 0;
#endif
  default:
    return 0;
  }

  if(mallopt(m_opt, value)) {
    *curr_val = (Sint) value;
    return 1;
  }

#endif /* #if HAVE_MALLOPT */

  return 0;
}

void
sys_alloc_stat(SysAllocStat *sasp)
{
   sasp->trim_threshold = trim_threshold;
   sasp->top_pad        = top_pad;
   sasp->mmap_threshold = mmap_threshold;
   sasp->mmap_max       = mmap_max;

}

#ifdef ERTS_SMP

/* Local system block state */

struct {
    int emergency;
    long emergency_timeout;
    erts_smp_cnd_t watchdog_cnd;
    erts_smp_tid_t watchdog_tid;
    int threads_to_block;
    int have_blocker;
    erts_smp_tid_t blocker_tid;
    int recursive_block;
    Uint32 allowed_activities;
    erts_smp_tsd_key_t blockable_key;
    erts_smp_mtx_t mtx;
    erts_smp_cnd_t cnd;
#ifdef ERTS_ENABLE_LOCK_CHECK
    int activity_changing;
    int checking;
#endif
} system_block_state;

/* Global system block state */
erts_system_block_state_t erts_system_block_state;


static ERTS_INLINE int
is_blockable_thread(void)
{
    return erts_smp_tsd_get(system_block_state.blockable_key) != NULL;
}

static ERTS_INLINE int
is_blocker(void)
{
    return (system_block_state.have_blocker
	    && erts_smp_equal_tids(system_block_state.blocker_tid,
				   erts_smp_thr_self()));
}

#ifdef ERTS_ENABLE_LOCK_CHECK
int
erts_lc_is_blocking(void)
{
    int res;
    erts_smp_mtx_lock(&system_block_state.mtx);
    res = erts_smp_pending_system_block() && is_blocker();
    erts_smp_mtx_unlock(&system_block_state.mtx);
    return res;
}
#endif

static ERTS_INLINE void
block_me(void (*prepare)(void *),
	 void (*resume)(void *),
	 void *arg,
	 int mtx_locked,
	 int want_to_block,
	 int update_act_changing,
	 profile_sched_msg_q *psmq)
{
    if (prepare)
	(*prepare)(arg);

    /* Locks might be held... */

    if (!mtx_locked)
	erts_smp_mtx_lock(&system_block_state.mtx);

    if (erts_smp_pending_system_block() && !is_blocker()) {
	int is_blockable = is_blockable_thread();
	ASSERT(is_blockable);

	if (is_blockable)
	    system_block_state.threads_to_block--;

	if (erts_system_profile_flags.scheduler && psmq) {
	    ErtsSchedulerData *esdp = erts_get_scheduler_data();
	    if (esdp) {
	    	profile_sched_msg *msg = NULL;
	        
		ASSERT(psmq->n < 2);
		msg = &((psmq->msg)[psmq->n]);
		msg->scheduler_id = esdp->no;
		get_now(&(msg->Ms), &(msg->s), &(msg->us));
		msg->no_schedulers = 0;
		msg->state = am_inactive;
	    	psmq->n++;
	    }
	}

#ifdef ERTS_ENABLE_LOCK_CHECK
	if (update_act_changing)
	    system_block_state.activity_changing--;
#endif

	erts_smp_cnd_broadcast(&system_block_state.cnd);

	do {
	    erts_smp_cnd_wait(&system_block_state.cnd, &system_block_state.mtx);
	} while (erts_smp_pending_system_block()
		 && !(want_to_block && !system_block_state.have_blocker));

#ifdef ERTS_ENABLE_LOCK_CHECK
	if (update_act_changing)
	    system_block_state.activity_changing++;
#endif
	if (erts_system_profile_flags.scheduler && psmq) {
	    ErtsSchedulerData *esdp = erts_get_scheduler_data();
	    if (esdp) {
	    	profile_sched_msg *msg = NULL;
	        
		ASSERT(psmq->n < 2);
		msg = &((psmq->msg)[psmq->n]);
		msg->scheduler_id = esdp->no;
		get_now(&(msg->Ms), &(msg->s), &(msg->us));
		msg->no_schedulers = 0;
		msg->state = am_active;
	    	psmq->n++;
	    }
	}

	if (is_blockable)
	    system_block_state.threads_to_block++;
    }

    if (!mtx_locked)
	erts_smp_mtx_unlock(&system_block_state.mtx);

    if (resume)
	(*resume)(arg);
}

void
erts_block_me(void (*prepare)(void *),
	      void (*resume)(void *),
	      void *arg)
{
    profile_sched_msg_q psmq;
    psmq.n = 0;
    if (prepare)
	(*prepare)(arg);

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_check_exact(NULL, 0); /* No locks should be locked */
#endif

    block_me(NULL, NULL, NULL, 0, 0, 0, &psmq);

    if (erts_system_profile_flags.scheduler && psmq.n > 0) 
    	dispatch_profile_msg_q(&psmq);

    if (resume)
	(*resume)(arg);
}

void
erts_register_blockable_thread(void)
{
    profile_sched_msg_q psmq;
    psmq.n = 0;
    if (!is_blockable_thread()) {
	erts_smp_mtx_lock(&system_block_state.mtx);
	system_block_state.threads_to_block++;
	erts_smp_tsd_set(system_block_state.blockable_key,
			 (void *) &erts_system_block_state);

	/* Someone might be waiting for us to block... */
	if (erts_smp_pending_system_block())
	    block_me(NULL, NULL, NULL, 1, 0, 0, &psmq);
	erts_smp_mtx_unlock(&system_block_state.mtx);

	if (erts_system_profile_flags.scheduler && psmq.n > 0)
	    dispatch_profile_msg_q(&psmq);
    }
}

void
erts_unregister_blockable_thread(void)
{
    if (is_blockable_thread()) {
	erts_smp_mtx_lock(&system_block_state.mtx);
	system_block_state.threads_to_block--;
	ASSERT(system_block_state.threads_to_block >= 0);
	erts_smp_tsd_set(system_block_state.blockable_key, NULL);

	/* Someone might be waiting for us to block... */
	if (erts_smp_pending_system_block())
	    erts_smp_cnd_broadcast(&system_block_state.cnd);
	erts_smp_mtx_unlock(&system_block_state.mtx);
    }
}

void
erts_note_activity_begin(erts_activity_t activity)
{
    erts_smp_mtx_lock(&system_block_state.mtx);
    if (erts_smp_pending_system_block()) {
	Uint32 broadcast = 0;
	switch (activity) {
	case ERTS_ACTIVITY_GC:
	    broadcast = (system_block_state.allowed_activities
			 & ERTS_BS_FLG_ALLOW_GC);
	    break;
	case ERTS_ACTIVITY_IO:
	    broadcast = (system_block_state.allowed_activities
			 & ERTS_BS_FLG_ALLOW_IO);
	    break;
	case ERTS_ACTIVITY_WAIT:
	    broadcast = 1;
	    break;
	default:
	    abort();
	    break;
	}
	if (broadcast)
	    erts_smp_cnd_broadcast(&system_block_state.cnd);
    }
    erts_smp_mtx_unlock(&system_block_state.mtx);
}

void
erts_check_block(erts_activity_t old_activity,
		 erts_activity_t new_activity,
		 int locked,
		 void (*prepare)(void *),
		 void (*resume)(void *),
		 void *arg)
{
    int do_block;
    profile_sched_msg_q psmq;

    psmq.n = 0;
    if (!locked && prepare)
	(*prepare)(arg);

    erts_smp_mtx_lock(&system_block_state.mtx);

    /* First check if it is ok to block... */
    if (!locked)
	do_block = 1;
    else {
	switch (old_activity) {
	case ERTS_ACTIVITY_UNDEFINED:
	    do_block = 0;
	    break;
	case ERTS_ACTIVITY_GC:
	    do_block = (system_block_state.allowed_activities
			& ERTS_BS_FLG_ALLOW_GC);
	    break;
	case ERTS_ACTIVITY_IO:
	    do_block = (system_block_state.allowed_activities
			& ERTS_BS_FLG_ALLOW_IO);
	    break;
	case ERTS_ACTIVITY_WAIT:
	    /* You are not allowed to leave activity waiting
	     * without supplying the possibility to block
	     * unlocked.
	     */
	    erts_set_activity_error(ERTS_ACT_ERR_LEAVE_WAIT_UNLOCKED,
				    __FILE__, __LINE__);
	    do_block = 0;
	    break;
	default:
	    erts_set_activity_error(ERTS_ACT_ERR_LEAVE_UNKNOWN_ACTIVITY,
				    __FILE__, __LINE__);
	    do_block = 0;
	    break;
	}
    }

    if (do_block) {
	/* ... then check if it is necessary to block... */

	switch (new_activity) {
	case ERTS_ACTIVITY_UNDEFINED:
	    do_block = 1;
	    break;
	case ERTS_ACTIVITY_GC:
	    do_block = !(system_block_state.allowed_activities
			 & ERTS_BS_FLG_ALLOW_GC);
	break;
	case ERTS_ACTIVITY_IO:
	    do_block = !(system_block_state.allowed_activities
			 & ERTS_BS_FLG_ALLOW_IO);
	    break;
	case ERTS_ACTIVITY_WAIT:
	    /* No need to block if we are going to wait */
	    do_block = 0;
	    break;
	default:
	    erts_set_activity_error(ERTS_ACT_ERR_ENTER_UNKNOWN_ACTIVITY,
				    __FILE__, __LINE__);
	    break;
	}
    }

    if (do_block) {

#ifdef ERTS_ENABLE_LOCK_CHECK
	if (!locked) {
	    /* Only system_block_state.mtx should be held */
	    erts_lc_check_exact(&system_block_state.mtx.lc, 1);
	}
#endif

	block_me(NULL, NULL, NULL, 1, 0, 1, &psmq);

    }

    erts_smp_mtx_unlock(&system_block_state.mtx);

    if (erts_system_profile_flags.scheduler && psmq.n > 0) 
	dispatch_profile_msg_q(&psmq);	

    if (!locked && resume)
	(*resume)(arg);
}



void
erts_set_activity_error(erts_activity_error_t error, char *file, int line)
{
    switch (error) {
    case ERTS_ACT_ERR_LEAVE_WAIT_UNLOCKED:
	erl_exit(1, "%s:%d: Fatal error: Leaving activity waiting without "
		 "supplying the possibility to block unlocked.",
		 file, line);
	break;
    case ERTS_ACT_ERR_LEAVE_UNKNOWN_ACTIVITY:
	erl_exit(1, "%s:%d: Fatal error: Leaving unknown activity.",
		 file, line);
	break;
    case ERTS_ACT_ERR_ENTER_UNKNOWN_ACTIVITY:
	erl_exit(1, "%s:%d: Fatal error: Leaving unknown activity.",
		 file, line);
	break;
    default:
	erl_exit(1, "%s:%d: Internal error in erts_smp_set_activity()",
		 file, line);
	break;
    }

}


static ERTS_INLINE int
threads_not_under_control(void)
{
    int res = system_block_state.threads_to_block;

    /* Waiting is always an allowed activity... */
    res -= erts_smp_atomic_read(&erts_system_block_state.in_activity.wait);

    if (system_block_state.allowed_activities & ERTS_BS_FLG_ALLOW_GC)
	res -= erts_smp_atomic_read(&erts_system_block_state.in_activity.gc);

    if (system_block_state.allowed_activities & ERTS_BS_FLG_ALLOW_IO)
	res -= erts_smp_atomic_read(&erts_system_block_state.in_activity.io);

    if (res < 0) {
	ASSERT(0);
	return 0;
    }
    return res;
}

/*
 * erts_block_system() blocks all threads registered as blockable.
 * It doesn't return until either all threads have blocked (0 is returned)
 * or it has timed out (ETIMEDOUT) is returned.
 *
 * If allowed activities == 0, blocked threads will release all locks
 * before blocking.
 *
 * If allowed_activities is != 0, erts_block_system() will allow blockable
 * threads to continue executing as long as they are doing an allowed
 * activity. When they are done with the allowed activity they will block,
 * *but* they will block holding locks. Therefore, the thread calling
 * erts_block_system() must *not* try to aquire any locks that might be
 * held by blocked threads holding locks from allowed activities.
 *
 * Currently allowed_activities are:
 *	* ERTS_BS_FLG_ALLOW_GC		Thread continues with garbage
 *					collection and blocks with
 *					main process lock on current
 *					process locked.
 *	* ERTS_BS_FLG_ALLOW_IO		Thread continues with I/O
 */

void
erts_block_system(Uint32 allowed_activities)
{
    int do_block;
    profile_sched_msg_q psmq;
    
    psmq.n = 0;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_check_exact(NULL, 0); /* No locks should be locked */
#endif

    erts_smp_mtx_lock(&system_block_state.mtx);

    do_block = erts_smp_pending_system_block();
    if (do_block
	&& system_block_state.have_blocker
	&& erts_smp_equal_tids(system_block_state.blocker_tid,
			       erts_smp_thr_self())) {
	ASSERT(system_block_state.recursive_block >= 0);
	system_block_state.recursive_block++;

	/* You are not allowed to restrict allowed activites
	   in a recursive block! */
	ERTS_SMP_LC_ASSERT((system_block_state.allowed_activities
			    & ~allowed_activities) == 0);
    }
    else {

	erts_smp_atomic_inc(&erts_system_block_state.do_block);

	/* Someone else might be waiting for us to block... */
	if (do_block) {
	do_block_me:
	    block_me(NULL, NULL, NULL, 1, 1, 0, &psmq);
	}

	ASSERT(!system_block_state.have_blocker);
	system_block_state.have_blocker = 1;
	system_block_state.blocker_tid = erts_smp_thr_self();
	system_block_state.allowed_activities = allowed_activities;

	if (is_blockable_thread())
	    system_block_state.threads_to_block--;

	while (threads_not_under_control() && !system_block_state.emergency)
	    erts_smp_cnd_wait(&system_block_state.cnd, &system_block_state.mtx);

	if (system_block_state.emergency) {
	    system_block_state.have_blocker = 0;
	    goto do_block_me;
	}
    }

    erts_smp_mtx_unlock(&system_block_state.mtx);

    if (erts_system_profile_flags.scheduler && psmq.n > 0 )
    	dispatch_profile_msg_q(&psmq);
}

/*
 * erts_emergency_block_system() should only be called when we are
 * about to write a crash dump...
 */

int
erts_emergency_block_system(long timeout, Uint32 allowed_activities)
{
    int res = 0;
    long another_blocker;

    erts_smp_mtx_lock(&system_block_state.mtx);

    if (system_block_state.emergency) {
	 /* Argh... */
	res = EINVAL;
	goto done;
    }

    another_blocker = erts_smp_pending_system_block();
    system_block_state.emergency = 1;
    erts_smp_atomic_inc(&erts_system_block_state.do_block);

    if (another_blocker) {
	if (is_blocker()) {
	    erts_smp_atomic_dec(&erts_system_block_state.do_block);
	    res = 0;
	    goto done;
	}
	/* kick the other blocker */
	erts_smp_cnd_broadcast(&system_block_state.cnd);
	while (system_block_state.have_blocker)
	    erts_smp_cnd_wait(&system_block_state.cnd, &system_block_state.mtx);
    }

    ASSERT(!system_block_state.have_blocker);
    system_block_state.have_blocker = 1;
    system_block_state.blocker_tid = erts_smp_thr_self();
    system_block_state.allowed_activities = allowed_activities;

    if (is_blockable_thread())
	system_block_state.threads_to_block--;

    if (timeout < 0) {
	while (threads_not_under_control())
	    erts_smp_cnd_wait(&system_block_state.cnd, &system_block_state.mtx);
    }
    else {	
	system_block_state.emergency_timeout = timeout;
	erts_smp_cnd_signal(&system_block_state.watchdog_cnd);

	while (system_block_state.emergency_timeout >= 0
	       && threads_not_under_control()) {
	    erts_smp_cnd_wait(&system_block_state.cnd,
			      &system_block_state.mtx);
	}
    }
 done:
    erts_smp_mtx_unlock(&system_block_state.mtx);
    return res;
}

void
erts_release_system(void)
{
    long do_block;
    profile_sched_msg_q psmq;
    
    psmq.n = 0;

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_check_exact(NULL, 0); /* No locks should be locked */
#endif

    erts_smp_mtx_lock(&system_block_state.mtx);
    ASSERT(is_blocker());

    ASSERT(system_block_state.recursive_block >= 0);

    if (system_block_state.recursive_block)
	system_block_state.recursive_block--;
    else {
	do_block = erts_smp_atomic_dectest(&erts_system_block_state.do_block);
	system_block_state.have_blocker = 0;
	if (is_blockable_thread())
	    system_block_state.threads_to_block++;
	else
	    do_block = 0;

	/* Someone else might be waiting for us to block... */
	if (do_block)
	    block_me(NULL, NULL, NULL, 1, 0, 0, &psmq);
	else
	    erts_smp_cnd_broadcast(&system_block_state.cnd);
    }

    erts_smp_mtx_unlock(&system_block_state.mtx);

    if (erts_system_profile_flags.scheduler && psmq.n > 0) 
    	dispatch_profile_msg_q(&psmq);
}

#ifdef ERTS_ENABLE_LOCK_CHECK

void
erts_lc_activity_change_begin(void)
{
    erts_smp_mtx_lock(&system_block_state.mtx);
    system_block_state.activity_changing++;
    erts_smp_mtx_unlock(&system_block_state.mtx);
}

void
erts_lc_activity_change_end(void)
{
    erts_smp_mtx_lock(&system_block_state.mtx);
    system_block_state.activity_changing--;
    if (system_block_state.checking && !system_block_state.activity_changing)
	erts_smp_cnd_broadcast(&system_block_state.cnd);
    erts_smp_mtx_unlock(&system_block_state.mtx);
}

#endif

int
erts_is_system_blocked(erts_activity_t allowed_activities)
{
    int blkd;

    erts_smp_mtx_lock(&system_block_state.mtx);
    blkd = (erts_smp_pending_system_block()
	    && system_block_state.have_blocker
	    && erts_smp_equal_tids(system_block_state.blocker_tid,
				   erts_smp_thr_self())
	    && !(system_block_state.allowed_activities & ~allowed_activities));
#ifdef ERTS_ENABLE_LOCK_CHECK
    if (blkd) {
	system_block_state.checking = 1;
	while (system_block_state.activity_changing)
	    erts_smp_cnd_wait(&system_block_state.cnd, &system_block_state.mtx);
	system_block_state.checking = 0;
	blkd = !threads_not_under_control();
    }
#endif
    erts_smp_mtx_unlock(&system_block_state.mtx);
    return blkd;
}

static void *
emergency_watchdog(void *unused)
{
    erts_smp_mtx_lock(&system_block_state.mtx);
    while (1) {
	long timeout;
	while (system_block_state.emergency_timeout < 0)
	    erts_smp_cnd_wait(&system_block_state.watchdog_cnd, &system_block_state.mtx);
	timeout = system_block_state.emergency_timeout;
	erts_smp_mtx_unlock(&system_block_state.mtx);

	if (erts_disable_tolerant_timeofday)
	    erts_milli_sleep(timeout);
	else {
	    SysTimeval to;
	    erts_get_timeval(&to);
	    to.tv_sec += timeout / 1000;
	    to.tv_usec += timeout % 1000;

	    while (1) {
		SysTimeval curr;
		erts_milli_sleep(timeout);
		erts_get_timeval(&curr);
		if (curr.tv_sec > to.tv_sec
		    || (curr.tv_sec == to.tv_sec && curr.tv_usec >= to.tv_usec)) {
		    break;
		}
		timeout = (to.tv_sec - curr.tv_sec)*1000;
		timeout += (to.tv_usec - curr.tv_usec)/1000;
	    }
	}

	erts_smp_mtx_lock(&system_block_state.mtx);
	system_block_state.emergency_timeout = -1;
	erts_smp_cnd_broadcast(&system_block_state.cnd);
    }
    erts_smp_mtx_unlock(&system_block_state.mtx);
    return NULL;
}

void
erts_system_block_init(void)
{
    erts_smp_thr_opts_t thr_opts = ERTS_SMP_THR_OPTS_DEFAULT_INITER;
    /* Local state... */
    system_block_state.emergency = 0;
    system_block_state.emergency_timeout = -1;
    erts_smp_cnd_init(&system_block_state.watchdog_cnd);
    system_block_state.threads_to_block = 0;
    system_block_state.have_blocker = 0;
    /* system_block_state.block_tid */
    system_block_state.recursive_block = 0;
    system_block_state.allowed_activities = 0;
    erts_smp_tsd_key_create(&system_block_state.blockable_key);
    erts_smp_mtx_init(&system_block_state.mtx, "system_block");
    erts_smp_cnd_init(&system_block_state.cnd);
#ifdef ERTS_ENABLE_LOCK_CHECK
    system_block_state.activity_changing = 0;
    system_block_state.checking = 0;
#endif

    thr_opts.suggested_stack_size = 8;
    erts_smp_thr_create(&system_block_state.watchdog_tid,
			emergency_watchdog,
			NULL,
			&thr_opts);

    /* Global state... */

    erts_smp_atomic_init(&erts_system_block_state.do_block, 0L);
    erts_smp_atomic_init(&erts_system_block_state.in_activity.wait, 0L);
    erts_smp_atomic_init(&erts_system_block_state.in_activity.gc, 0L);
    erts_smp_atomic_init(&erts_system_block_state.in_activity.io, 0L);

    /* Make sure blockable threads unregister when exiting... */
    erts_smp_install_exit_handler(erts_unregister_blockable_thread);
}


#endif /* #ifdef ERTS_SMP */

char *
erts_read_env(char *key)
{
    size_t value_len = 256;
    char *value = erts_alloc(ERTS_ALC_T_TMP, value_len);
    int res;
    while (1) {
	res = erts_sys_getenv(key, value, &value_len);
	if (res <= 0)
	    break;
	value = erts_realloc(ERTS_ALC_T_TMP, value, value_len);
    }
    if (res != 0) {
	erts_free(ERTS_ALC_T_TMP, value);
	return NULL;
    }
    return value;
}

void
erts_free_read_env(void *value)
{
    if (value)
	erts_free(ERTS_ALC_T_TMP, value);
}

int
erts_write_env(char *key, char *value)
{
    int ix, res;
    size_t key_len = sys_strlen(key), value_len = sys_strlen(value);
    char *key_value = erts_alloc_fnf(ERTS_ALC_T_TMP,
				     key_len + 1 + value_len + 1);
    if (!key_value) {
	errno = ENOMEM;
	return -1;
    }
    sys_memcpy((void *) key_value, (void *) key, key_len);
    ix = key_len;
    key_value[ix++] = '=';
    sys_memcpy((void *) key_value, (void *) value, value_len);
    ix += value_len;
    key_value[ix] = '\0';
    res = erts_sys_putenv(key_value, key_len);
    erts_free(ERTS_ALC_T_TMP, key_value);
    return res;
}

/*
 * To be used to silence unused result warnings, but do not abuse it.
 */
void erts_silence_warn_unused_result(long unused)
{

}

#ifdef DEBUG
/*
 * Handy functions when using a debugger - don't use in the code!
 */

void upp(buf,sz)
byte* buf;
int sz;
{
    bin_write(ERTS_PRINT_STDERR,NULL,buf,sz);
}

void pat(Eterm atom)
{
    upp(atom_tab(atom_val(atom))->name,
	atom_tab(atom_val(atom))->len);
}


void pinfo()
{
    process_info(ERTS_PRINT_STDOUT, NULL);
}


void pp(p)
Process *p;
{
    if(p)
	print_process_info(ERTS_PRINT_STDERR, NULL, p);
}
    
void ppi(Eterm pid)
{
    pp(erts_pid2proc_unlocked(pid));
}

void td(Eterm x)
{
    erts_fprintf(stderr, "%T\n", x);
}

void
ps(Process* p, Eterm* stop)
{
    Eterm* sp = STACK_START(p) - 1;

    if (stop <= STACK_END(p)) {
        stop = STACK_END(p) + 1;
    }

    while(sp >= stop) {
	erts_printf("%p: %.75T\n", sp, *sp);
	sp--;
    }
}
#endif


