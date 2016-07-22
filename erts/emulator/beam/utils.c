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
#include "erl_map.h"
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
#include "erl_thr_progress.h"
#include "erl_thr_queue.h"
#include "erl_sched_spec_pre_alloc.h"
#include "beam_bp.h"
#include "erl_ptab.h"
#include "erl_check_io.h"
#include "erl_bif_unique.h"
#define ERTS_WANT_TIMER_WHEEL_API
#include "erl_time.h"
#ifdef HIPE
#  include "hipe_mode_switch.h"
#endif

#undef M_TRIM_THRESHOLD
#undef M_TOP_PAD
#undef M_MMAP_THRESHOLD
#undef M_MMAP_MAX

#if defined(__GLIBC__) && defined(HAVE_MALLOC_H)
#include <malloc.h>
#endif

#if !defined(HAVE_MALLOPT)
#undef  HAVE_MALLOPT
#define HAVE_MALLOPT 0
#endif

Eterm*
erts_heap_alloc(Process* p, Uint need, Uint xtra)
{
    ErlHeapFragment* bp;
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

    n = need + xtra;
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

    bp->next = MBUF(p);
    MBUF(p) = bp;
    bp->alloc_size = n;
    bp->used_size = need;
    MBUF_SIZE(p) += n;
    bp->off_heap.first = NULL;
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
erl_grow_estack(ErtsEStack* s, Uint need)
{
    Uint old_size = (s->end - s->start);
    Uint new_size;
    Uint sp_offs = s->sp - s->start;

    if (need < old_size)
	new_size = 2*old_size;
    else
	new_size = ((need / old_size) + 2) * old_size;

    if (s->start != s->edefault) {
	s->start = erts_realloc(s->alloc_type, s->start,
				new_size*sizeof(Eterm));
    } else {
	Eterm* new_ptr = erts_alloc(s->alloc_type, new_size*sizeof(Eterm));
	sys_memcpy(new_ptr, s->start, old_size*sizeof(Eterm));
	s->start = new_ptr;
    }
    s->end = s->start + new_size;
    s->sp = s->start + sp_offs;
}
/*
 * Helper function for the WSTACK macros defined in global.h.
 */
void
erl_grow_wstack(ErtsWStack* s, Uint need)
{
    Uint old_size = (s->wend - s->wstart);
    Uint new_size;
    Uint sp_offs = s->wsp - s->wstart;

    if (need < old_size)
	new_size = 2 * old_size;
    else
	new_size = ((need / old_size) + 2) * old_size;

    if (s->wstart != s->wdefault) {
	s->wstart = erts_realloc(s->alloc_type, s->wstart,
				 new_size*sizeof(UWord));
    } else {
	UWord* new_ptr = erts_alloc(s->alloc_type, new_size*sizeof(UWord));
	sys_memcpy(new_ptr, s->wstart, old_size*sizeof(UWord));
	s->wstart = new_ptr;
    }
    s->wend = s->wstart + new_size;
    s->wsp = s->wstart + sp_offs;
}

/*
 * Helper function for the PSTACK macros defined in global.h.
 */
void
erl_grow_pstack(ErtsPStack* s, void* default_pstack, unsigned need_bytes)
{
    Uint old_size = s->pend - s->pstart;
    Uint new_size;
    Uint sp_offs = s->psp - s->pstart;

    if (need_bytes < old_size)
	new_size = 2 * old_size;
    else
	new_size = ((need_bytes / old_size) + 2) * old_size;

    if (s->pstart != default_pstack) {
	s->pstart = erts_realloc(s->alloc_type, s->pstart, new_size);
    } else {
	byte* new_ptr = erts_alloc(s->alloc_type, new_size);
	sys_memcpy(new_ptr, s->pstart, old_size);
	s->pstart = new_ptr;
    }
    s->pend = s->pstart + new_size;
    s->psp = s->pstart + sp_offs;
}

/*
 * Helper function for the EQUEUE macros defined in global.h.
 */

void
erl_grow_equeue(ErtsEQueue* q, Eterm* default_equeue)
{
    Uint old_size = (q->end - q->start);
    Uint new_size = old_size * 2;
    Uint first_part = (q->end - q->front);
    Uint second_part = (q->back - q->start);
    Eterm* new_ptr = erts_alloc(q->alloc_type, new_size*sizeof(Eterm));
    ASSERT(q->back == q->front);   // of course the queue is full now!
    if (first_part > 0)
      sys_memcpy(new_ptr, q->front, first_part*sizeof(Eterm));
    if (second_part > 0)
      sys_memcpy(new_ptr+first_part, q->start, second_part*sizeof(Eterm));
    if (q->start != default_equeue)
      erts_free(q->alloc_type, q->start);
    q->start = new_ptr;
    q->end = q->start + new_size;
    q->front = q->start;
    q->back = q->start + old_size;
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
Sint
erts_list_length(Eterm list)
{
    Sint i = 0;

    while(is_list(list)) {
	i++;
	list = CDR(list_val(list));
    }
    if (is_not_nil(list)) {
	return -1;
    }
    return i;
}

static const struct {
    Sint64 mask;
    int bits;
} fib_data[] = {{ERTS_I64_LITERAL(0x2), 1},
		{ERTS_I64_LITERAL(0xc), 2},
		{ERTS_I64_LITERAL(0xf0), 4},
		{ERTS_I64_LITERAL(0xff00), 8},
		{ERTS_I64_LITERAL(0xffff0000), 16},
		{ERTS_I64_LITERAL(0xffffffff00000000), 32}};

static ERTS_INLINE int
fit_in_bits(Sint64 value, int start)
{
    int bits = 0;
    int i;

    for (i = start; i >= 0; i--) {
	if (value & fib_data[i].mask) {
	    value >>= fib_data[i].bits;
	    bits |= fib_data[i].bits;
	}
    }

    bits++;

    return bits;
}

int erts_fit_in_bits_int64(Sint64 value)
{
    return fit_in_bits(value, 5);
}

int erts_fit_in_bits_int32(Sint32 value)
{
    return fit_in_bits((Sint64) (Uint32) value, 4);
}

int erts_fit_in_bits_uint(Uint value)
{
#if ERTS_SIZEOF_ETERM == 4
    return fit_in_bits((Sint64) (Uint32) value, 4);
#elif ERTS_SIZEOF_ETERM == 8
    return fit_in_bits(value, 5);
#else
# error "No way, Jose"
#endif
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
	return erts_atom_put((byte *) str, sys_strlen(str), ERTS_ATOM_ENC_LATIN1, 1);
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
	    *szp += ERTS_UINT64_HEAP_SIZE(ui64);
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
	    *szp += ERTS_SINT64_HEAP_SIZE(si64);
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
erts_bld_atom_uword_2tup_list(Uint **hpp, Uint *szp,
                              Sint length, Eterm atoms[], UWord uints[])
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
**  h(i) = h(i-1)*X + B(i-1)
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
#define FUNNY_NUMBER13 268440593
#define FUNNY_NUMBER14 268440611

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

#define MAKE_HASH_TUPLE_OP      (FIRST_VACANT_TAG_DEF)
#define MAKE_HASH_TERM_ARRAY_OP (FIRST_VACANT_TAG_DEF+1)
#define MAKE_HASH_CDR_PRE_OP    (FIRST_VACANT_TAG_DEF+2)
#define MAKE_HASH_CDR_POST_OP   (FIRST_VACANT_TAG_DEF+3)

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
		(((((hash)*(Prime1) + (x & 0xFF)) * (Prime1) +	        \
		((x >> 8) & 0xFF)) * (Prime1) +			        \
		((x >> 16) & 0xFF)) * (Prime1) +			\
		 (x >> 24));						\
	} while(0)

#define UINT32_HASH_RET(Expr, Prime1, Prime2)           \
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
#if defined(ARCH_64)
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
		    WSTACK_PUSH3(stack, (UWord) &funp->env[1], (num_free-1), MAKE_HASH_TERM_ARRAY_OP);
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
            if (ff.fd == 0.0f) {
                /* ensure positive 0.0 */
                ff.fd = erts_get_positive_zero_float();
            }
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
#if defined(ARCH_64)
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
    case MAP_DEF:
        hash = hash*FUNNY_NUMBER13 + FUNNY_NUMBER14 + make_hash2(term);
        break;
    case TUPLE_DEF:
	{
	    Eterm* ptr = tuple_val(term);
	    Uint arity = arityval(*ptr);

	    WSTACK_PUSH3(stack, (UWord) arity, (UWord)(ptr+1), (UWord) arity);
	    op = MAKE_HASH_TUPLE_OP;
	}/*fall through*/
    case MAKE_HASH_TUPLE_OP:
    case MAKE_HASH_TERM_ARRAY_OP:
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
	erts_exit(ERTS_ERROR_EXIT, "Invalid tag in make_hash(0x%X,0x%X)\n", term, op);
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
    Uint32 hash_xor_pairs;
    DeclareTmpHeapNoproc(tmp_big,2);

    ERTS_UNDEF(hash_xor_pairs, 0);

/* (HCONST * {2, ..., 22}) mod 2^32 */
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
#define HCONST_16 0xe3779b90UL
#define HCONST_17 0x81af1549UL
#define HCONST_18 0x1fe68f02UL
#define HCONST_19 0xbe1e08bbUL
#define HCONST_20 0x5c558274UL
#define HCONST_21 0xfa8cfc2dUL
#define HCONST_22 0x98c475e6UL

#define HASH_MAP_TAIL (_make_header(1,_TAG_HEADER_REF))
#define HASH_MAP_PAIR (_make_header(2,_TAG_HEADER_REF))
#define HASH_CDR      (_make_header(3,_TAG_HEADER_REF))

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
	    }                                     \
	    UINT32_HASH(y, AConst);               \
	} while(0)

#define IS_SSMALL28(x) (((Uint) (((x) >> (28-1)) + 1)) < 2)

#ifdef ARCH_64
#  define POINTER_HASH(Ptr, AConst) UINT32_HASH_2((Uint32)(UWord)(Ptr), (((UWord)(Ptr)) >> 32), AConst)
#else
#  define POINTER_HASH(Ptr, AConst) UINT32_HASH(Ptr, AConst)
#endif

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
		tmp = CDR(ptr);
                ESTACK_PUSH(s, tmp);
		term = CAR(ptr);
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
		for (i = arity; ; i--) {
		    term = elem[i];
                    if (i == 1)
                        break;
                    ESTACK_PUSH(s, term);
		}
	    }
	    break;
            case MAP_SUBTAG:
            {
                Eterm* ptr = boxed_val(term) + 1;
                Uint size;
                int i;
                switch (hdr & _HEADER_MAP_SUBTAG_MASK) {
                case HAMT_SUBTAG_HEAD_FLATMAP:
                {
                    flatmap_t *mp = (flatmap_t *)flatmap_val(term);
                    Eterm *ks = flatmap_get_keys(mp);
                    Eterm *vs = flatmap_get_values(mp);
                    size      = flatmap_get_size(mp);
                    UINT32_HASH(size, HCONST_16);
                    if (size == 0)
                        goto hash2_common;

                    /* We want a portable hash function that is *independent* of
                     * the order in which keys and values are encountered.
                     * We therefore calculate context independent hashes for all    				      .
                     * key-value pairs and then xor them together.
                     */
                    ESTACK_PUSH(s, hash_xor_pairs);
                    ESTACK_PUSH(s, hash);
                    ESTACK_PUSH(s, HASH_MAP_TAIL);
                    hash = 0;
                    hash_xor_pairs = 0;
                    for (i = size - 1; i >= 0; i--) {
                        ESTACK_PUSH(s, HASH_MAP_PAIR);
                        ESTACK_PUSH(s, vs[i]);
                        ESTACK_PUSH(s, ks[i]);
                    }
                    goto hash2_common;
                }

                case HAMT_SUBTAG_HEAD_ARRAY:
                case HAMT_SUBTAG_HEAD_BITMAP:
                    size = *ptr++;
                    UINT32_HASH(size, HCONST_16);
                    if (size == 0)
                        goto hash2_common;
                    ESTACK_PUSH(s, hash_xor_pairs);
                    ESTACK_PUSH(s, hash);
                    ESTACK_PUSH(s, HASH_MAP_TAIL);
                    hash = 0;
                    hash_xor_pairs = 0;
                }
                switch (hdr & _HEADER_MAP_SUBTAG_MASK) {
                case HAMT_SUBTAG_HEAD_ARRAY:
                    i = 16;
                    break;
                case HAMT_SUBTAG_HEAD_BITMAP:
                case HAMT_SUBTAG_NODE_BITMAP:
                    i = hashmap_bitcount(MAP_HEADER_VAL(hdr));
                    break;
                default:
                    erts_exit(ERTS_ERROR_EXIT, "bad header");
                }
                while (i) {
                    if (is_list(*ptr)) {
                        Eterm* cons = list_val(*ptr);
                        ESTACK_PUSH(s, HASH_MAP_PAIR);
                        ESTACK_PUSH(s, CDR(cons));
                        ESTACK_PUSH(s, CAR(cons));
                    }
                    else {
                        ASSERT(is_boxed(*ptr));
                        ESTACK_PUSH(s, *ptr);
                    }
                    i--; ptr++;
                }
                goto hash2_common;
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
                    ASSERT(i < n);
		    t = BIG_DIGIT(ptr, i++);
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
                if (ff.fd == 0.0f) {
                    /* ensure positive 0.0 */
                    ff.fd = erts_get_positive_zero_float();
                }
#if defined(WORDS_BIGENDIAN) || defined(DOUBLE_MIDDLE_ENDIAN)
		UINT32_HASH_2(ff.fw[0], ff.fw[1], HCONST_12);
#else
		UINT32_HASH_2(ff.fw[1], ff.fw[0], HCONST_12);
#endif
		goto hash2_common;
	    }
	    break;

	    default:
		erts_exit(ERTS_ERROR_EXIT, "Invalid tag in make_hash2(0x%X)\n", term);
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
		    erts_exit(ERTS_ERROR_EXIT, "Invalid tag in make_hash2(0x%X)\n", term);
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
	    erts_exit(ERTS_ERROR_EXIT, "Invalid tag in make_hash2(0x%X)\n", term);
	hash2_common:

	    /* Uint32 hash always has the hash value of the previous term,
	     * compounded or otherwise.
	     */

	    if (ESTACK_ISEMPTY(s)) {
		DESTROY_ESTACK(s);
		UnUseTmpHeapNoproc(2);
		return hash;
	    }

	    term = ESTACK_POP(s);

	    switch (term) {
		case HASH_MAP_TAIL: {
		    hash = (Uint32) ESTACK_POP(s);
                    UINT32_HASH(hash_xor_pairs, HCONST_19);
		    hash_xor_pairs = (Uint32) ESTACK_POP(s);
		    goto hash2_common;
		}
		case HASH_MAP_PAIR:
		    hash_xor_pairs ^= hash;
                    hash = 0;
		    goto hash2_common;
		default:
		    break;
	    }
	}
    }
    }
}

/* Term hash function for internal use.
 *
 * Limitation #1: Is not "portable" in any way between different VM instances.
 *
 * Limitation #2: The hash value is only valid as long as the term exists
 * somewhere in the VM. Why? Because external pids, ports and refs are hashed
 * by mixing the node *pointer* value. If a node disappears and later reappears
 * with a new ErlNode struct, externals from that node will hash different than
 * before.
 *
 * One IMPORTANT property must hold (for hamt).
 * EVERY BIT of the term that is significant for equality (see EQ)
 * MUST BE USED AS INPUT FOR THE HASH. Two different terms must always have a
 * chance of hashing different when salted: hash([Salt|A]) vs hash([Salt|B]).
 *
 * This is why we can not use cached hash values for atoms for example.
 *
 */

#define CONST_HASH(AConst)                              \
do {  /* Lightweight mixing of constant (type info) */  \
    hash ^= AConst;                                     \
    hash = (hash << 17) ^ (hash >> (32-17));            \
} while (0)

Uint32
make_internal_hash(Eterm term)
{
    Uint32 hash;
    Uint32 hash_xor_pairs;

    ERTS_UNDEF(hash_xor_pairs, 0);

    /* Optimization. Simple cases before declaration of estack. */
    if (primary_tag(term) == TAG_PRIMARY_IMMED1) {
        hash = 0;
    #if ERTS_SIZEOF_ETERM == 8
        UINT32_HASH_2((Uint32)term, (Uint32)(term >> 32), HCONST);
    #elif ERTS_SIZEOF_ETERM == 4
        UINT32_HASH(term, HCONST);
    #else
    #  error "No you don't"
    #endif
        return hash;
    }
    {
    Eterm tmp;
    DECLARE_ESTACK(s);

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
                UINT32_HASH_2(sh, (Uint32)c, HCONST_22);

	    if (is_list(term)) {
		tmp = CDR(ptr);
                CONST_HASH(HCONST_17);  /* Hash CAR in cons cell */
                ESTACK_PUSH(s, tmp);
                if (is_not_list(tmp)) {
                    ESTACK_PUSH(s, HASH_CDR);
                }
		term = CAR(ptr);
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
		    goto pop_next;
		for (i = arity; ; i--) {
		    term = elem[i];
                    if (i == 1)
                        break;
                    ESTACK_PUSH(s, term);
		}
	    }
	    break;

            case MAP_SUBTAG:
            {
                Eterm* ptr = boxed_val(term) + 1;
                Uint size;
                int i;
                switch (hdr & _HEADER_MAP_SUBTAG_MASK) {
                case HAMT_SUBTAG_HEAD_FLATMAP:
                {
                    flatmap_t *mp = (flatmap_t *)flatmap_val(term);
                    Eterm *ks = flatmap_get_keys(mp);
                    Eterm *vs = flatmap_get_values(mp);
                    size      = flatmap_get_size(mp);
                    UINT32_HASH(size, HCONST_16);
                    if (size == 0)
                        goto pop_next;

                    /* We want a hash function that is *independent* of
                     * the order in which keys and values are encountered.
                     * We therefore calculate context independent hashes for all    				      .
                     * key-value pairs and then xor them together.
                     *
                     * We *do* need to use an initial seed for each pair, i.e. the
                     * hash value, so the hash value is reset for each pair with 'hash'.
                     * If we don't, no additional entropy is given to the system and the
                     * hash collision resolution in maps:from_list/1 would fail.
                     */
                    ESTACK_PUSH(s, hash_xor_pairs);
                    ESTACK_PUSH(s, hash);
                    ESTACK_PUSH(s, HASH_MAP_TAIL);
                    for (i = size - 1; i >= 0; i--) {
                        ESTACK_PUSH(s, hash); /* initial seed for all pairs */
                        ESTACK_PUSH(s, HASH_MAP_PAIR);
                        ESTACK_PUSH(s, vs[i]);
                        ESTACK_PUSH(s, ks[i]);
                    }
                    hash_xor_pairs = 0;
                    goto pop_next;
                }
                case HAMT_SUBTAG_HEAD_ARRAY:
                case HAMT_SUBTAG_HEAD_BITMAP:
                    size = *ptr++;
                    UINT32_HASH(size, HCONST_16);
                    if (size == 0)
                        goto pop_next;
                    ESTACK_PUSH(s, hash_xor_pairs);
                    ESTACK_PUSH(s, hash);
                    ESTACK_PUSH(s, HASH_MAP_TAIL);
                    hash_xor_pairs = 0;
                }
                switch (hdr & _HEADER_MAP_SUBTAG_MASK) {
                case HAMT_SUBTAG_HEAD_ARRAY:
                    i = 16;
                    break;
                case HAMT_SUBTAG_HEAD_BITMAP:
                case HAMT_SUBTAG_NODE_BITMAP:
                    i = hashmap_bitcount(MAP_HEADER_VAL(hdr));
                    break;
                default:
                    erts_exit(ERTS_ERROR_EXIT, "bad header");
                }
                while (i) {
                    if (is_list(*ptr)) {
                        Eterm* cons = list_val(*ptr);
                        ESTACK_PUSH(s, hash); /* initial seed for all pairs */
                        ESTACK_PUSH(s, HASH_MAP_PAIR);
                        ESTACK_PUSH(s, CDR(cons));
                        ESTACK_PUSH(s, CAR(cons));
                    }
                    else {
                        ASSERT(is_boxed(*ptr));
                        ESTACK_PUSH(s, *ptr);
                    }
                    i--; ptr++;
                }
                goto pop_next;
            }
            break;
	    case EXPORT_SUBTAG:
	    {
		Export* ep = *((Export **) (export_val(term) + 1));
                /* Assumes Export entries never moves */
                POINTER_HASH(ep, HCONST_14);
		goto pop_next;
	    }

	    case FUN_SUBTAG:
	    {
		ErlFunThing* funp = (ErlFunThing *) fun_val(term);
		Uint num_free = funp->num_free;
                UINT32_HASH_2(num_free, funp->fe->module, HCONST_20);
                UINT32_HASH_2(funp->fe->old_index, funp->fe->old_uniq, HCONST_21);
		if (num_free == 0) {
		    goto pop_next;
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
		goto pop_next;
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
                    ASSERT(i < n);
		    t = BIG_DIGIT(ptr, i++);
		    x = t & 0xffffffff;
		    y = t >> 32;
		    UINT32_HASH_2(x, y, con);
		} while (i < n);
#else
#error "unsupported D_EXP size"
#endif
		goto pop_next;
	    }
	    break;
	    case REF_SUBTAG:
		UINT32_HASH(internal_ref_numbers(term)[0], HCONST_7);
                ASSERT(internal_ref_no_of_numbers(term) == 3);
                UINT32_HASH_2(internal_ref_numbers(term)[1],
                              internal_ref_numbers(term)[2], HCONST_8);
                goto pop_next;

            case EXTERNAL_REF_SUBTAG:
            {
                ExternalThing* thing = external_thing_ptr(term);

                ASSERT(external_thing_ref_no_of_numbers(thing) == 3);
                /* See limitation #2 */
            #ifdef ARCH_64
                POINTER_HASH(thing->node, HCONST_7);
                UINT32_HASH(external_thing_ref_numbers(thing)[0], HCONST_7);
            #else
                UINT32_HASH_2(thing->node,
                              external_thing_ref_numbers(thing)[0], HCONST_7);
            #endif
                UINT32_HASH_2(external_thing_ref_numbers(thing)[1],
                              external_thing_ref_numbers(thing)[2], HCONST_8);
                goto pop_next;
            }
            case EXTERNAL_PID_SUBTAG: {
                ExternalThing* thing = external_thing_ptr(term);
                /* See limitation #2 */
            #ifdef ARCH_64
                POINTER_HASH(thing->node, HCONST_5);
                UINT32_HASH(thing->data.ui[0], HCONST_5);
            #else
                UINT32_HASH_2(thing->node, thing->data.ui[0], HCONST_5);
            #endif
		goto pop_next;
            }
	    case EXTERNAL_PORT_SUBTAG: {
                ExternalThing* thing = external_thing_ptr(term);
                /* See limitation #2 */
            #ifdef ARCH_64
                POINTER_HASH(thing->node, HCONST_6);
                UINT32_HASH(thing->data.ui[0], HCONST_6);
            #else
                UINT32_HASH_2(thing->node, thing->data.ui[0], HCONST_6);
            #endif
		goto pop_next;
            }
	    case FLOAT_SUBTAG:
	    {
		FloatDef ff;
		GET_DOUBLE(term, ff);
                if (ff.fd == 0.0f) {
                    /* ensure positive 0.0 */
                    ff.fd = erts_get_positive_zero_float();
                }
		UINT32_HASH_2(ff.fw[0], ff.fw[1], HCONST_12);
		goto pop_next;
	    }
	    default:
		erts_exit(ERTS_ERROR_EXIT, "Invalid tag in make_hash2(0x%X)\n", term);
	    }
	}
	break;
        case TAG_PRIMARY_IMMED1:
        #if ERTS_SIZEOF_ETERM == 8
            UINT32_HASH_2((Uint32)term, (Uint32)(term >> 32), HCONST);
        #else
            UINT32_HASH(term, HCONST);
        #endif
            goto pop_next;

	default:
	    erts_exit(ERTS_ERROR_EXIT, "Invalid tag in make_hash2(0x%X)\n", term);

	pop_next:
	    if (ESTACK_ISEMPTY(s)) {
		DESTROY_ESTACK(s);

		return hash;
	    }

	    term = ESTACK_POP(s);

	    switch (term) {
		case HASH_MAP_TAIL: {
		    hash = (Uint32) ESTACK_POP(s);
                    UINT32_HASH(hash_xor_pairs, HCONST_19);
		    hash_xor_pairs = (Uint32) ESTACK_POP(s);
		    goto pop_next;
		}
		case HASH_MAP_PAIR:
		    hash_xor_pairs ^= hash;
		    hash = (Uint32) ESTACK_POP(s); /* initial seed for all pairs */
		    goto pop_next;

	        case HASH_CDR:
		    CONST_HASH(HCONST_18);   /* Hash CDR i cons cell */
		    goto pop_next;
		default:
		    break;
	    }
	}
    }
    }

#undef CONST_HASH
#undef HASH_MAP_TAIL
#undef HASH_MAP_PAIR
#undef HASH_CDR

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
#if defined(ARCH_64)
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
	    if (y3) {
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
		    WSTACK_PUSH3(stack, (UWord) &funp->env[1], (num_free-1), MAKE_HASH_TERM_ARRAY_OP);
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
            if (ff.fd == 0.0f) {
                /* ensure positive 0.0 */
                ff.fd = erts_get_positive_zero_float();
            }
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

    case MAP_DEF:
        hash = hash*FUNNY_NUMBER13 + FUNNY_NUMBER14 + make_hash2(term);
        break;
    case TUPLE_DEF:
	{
	    Eterm* ptr = tuple_val(term);
	    Uint arity = arityval(*ptr);

	    WSTACK_PUSH3(stack, (UWord) arity, (UWord) (ptr+1), (UWord) arity);
	    op = MAKE_HASH_TUPLE_OP;
	}/*fall through*/
    case MAKE_HASH_TUPLE_OP:
    case MAKE_HASH_TERM_ARRAY_OP:
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
	erts_exit(ERTS_ERROR_EXIT, "Invalid tag in make_broken_hash\n");
	return 0;
      }
      if (WSTACK_ISEMPTY(stack)) break;
      op = (Uint) WSTACK_POP(stack);
    }

    DESTROY_WSTACK(stack);
    return hash;

#undef MAKE_HASH_TUPLE_OP
#undef MAKE_HASH_TERM_ARRAY_OP
#undef MAKE_HASH_CDR_PRE_OP
#undef MAKE_HASH_CDR_POST_OP
}

static Eterm
do_allocate_logger_message(Eterm gleader, Eterm **hp, ErlOffHeap **ohp,
			   ErlHeapFragment **bp, Process **p, Uint sz)
{
    Uint gl_sz;
    gl_sz = IS_CONST(gleader) ? 0 : size_object(gleader);
    sz = sz + gl_sz;

#ifndef ERTS_SMP
#ifdef USE_THREADS
    if (!erts_get_scheduler_data()) /* Must be scheduler thread */
	*p = NULL;
    else
#endif
    {
	*p = erts_whereis_process(NULL, 0, am_error_logger, 0, 0);
	if (*p) {
	    erts_aint32_t state = erts_smp_atomic32_read_acqb(&(*p)->state);
	    if (state & (ERTS_PSFLG_RUNNING|ERTS_PSFLG_RUNNING_SYS))
		*p = NULL;
	}
    }

    if (!*p) {
	return NIL;
    }

    /* So we have an error logger, lets build the message */
#endif
    *bp = new_message_buffer(sz);
    *ohp = &(*bp)->off_heap;
    *hp = (*bp)->mem;

    return (is_nil(gleader)
	  ? am_noproc
	  : (IS_CONST(gleader)
	     ? gleader
	     : copy_struct(gleader,gl_sz,hp,*ohp)));
}

static void do_send_logger_message(Eterm *hp, ErlOffHeap *ohp, ErlHeapFragment *bp,
				   Process *p, Eterm message)
{
#ifdef HARDDEBUG
    erts_fprintf(stderr, "%T\n", message);
#endif
#ifdef ERTS_SMP
    {
	Eterm from = erts_get_current_pid();
	if (is_not_internal_pid(from))
	    from = NIL;
	erts_queue_error_logger_message(from, message, bp);
    }
#else
    {
	ErtsMessage *mp = erts_alloc_message(0, NULL);
	mp->data.heap_frag = bp;
	erts_queue_message(p, 0, mp, message, am_system);
    }
#endif
}

/* error_logger !
   {notify,{info_msg,gleader,{emulator,format,[args]}}} |
   {notify,{error,gleader,{emulator,format,[args]}}} |
   {notify,{warning_msg,gleader,{emulator,format,[args}]}} */
static int do_send_to_logger(Eterm tag, Eterm gleader, char *buf, int len)
{
    Uint sz;
    Eterm gl;
    Eterm list,args,format,tuple1,tuple2,tuple3;

    Eterm *hp = NULL;
    ErlOffHeap *ohp = NULL;
    ErlHeapFragment *bp = NULL;
    Process *p = NULL;

    ASSERT(is_atom(tag));

    if (len <= 0) {
	return -1;
    }

    sz = len * 2 /* message list */ + 2 /* cons surrounding message list */
	+ 3 /*outer 2-tuple*/ + 4 /* middle 3-tuple */ + 4 /*inner 3-tuple */
	+ 8 /* "~s~n" */;

    /* gleader size is accounted and allocated next */
    gl = do_allocate_logger_message(gleader, &hp, &ohp, &bp, &p, sz);

    if(is_nil(gl)) {
       /* buf *always* points to a null terminated string */
       erts_fprintf(stderr, "(no error logger present) %T: \"%s\"\n",
                    tag, buf);
       return 0;
    }

    list = buf_to_intlist(&hp, buf, len, NIL);
    args = CONS(hp,list,NIL);
    hp += 2;
    format = buf_to_intlist(&hp, "~s~n", 4, NIL);
    tuple1 = TUPLE3(hp, am_emulator, format, args);
    hp += 4;
    tuple2 = TUPLE3(hp, tag, gl, tuple1);
    hp += 4;
    tuple3 = TUPLE2(hp, am_notify, tuple2);

    do_send_logger_message(hp, ohp, bp, p, tuple3);
    return 0;
}

static int do_send_term_to_logger(Eterm tag, Eterm gleader,
				  char *buf, int len, Eterm args)
{
    Uint sz;
    Eterm gl;
    Uint args_sz;
    Eterm format,tuple1,tuple2,tuple3;

    Eterm *hp = NULL;
    ErlOffHeap *ohp = NULL;
    ErlHeapFragment *bp = NULL;
    Process *p = NULL;

    ASSERT(is_atom(tag));

    args_sz = size_object(args);
    sz = len * 2 /* format */ + args_sz
	+ 3 /*outer 2-tuple*/ + 4 /* middle 3-tuple */ + 4 /*inner 3-tuple */;

    /* gleader size is accounted and allocated next */
    gl = do_allocate_logger_message(gleader, &hp, &ohp, &bp, &p, sz);

    if(is_nil(gl)) {
       /* buf *always* points to a null terminated string */
       erts_fprintf(stderr, "(no error logger present) %T: \"%s\" %T\n",
                    tag, buf, args);
       return 0;
    }

    format = buf_to_intlist(&hp, buf, len, NIL);
    args = copy_struct(args, args_sz, &hp, ohp);
    tuple1 = TUPLE3(hp, am_emulator, format, args);
    hp += 4;
    tuple2 = TUPLE3(hp, tag, gl, tuple1);
    hp += 4;
    tuple3 = TUPLE2(hp, am_notify, tuple2);

    do_send_logger_message(hp, ohp, bp, p, tuple3);
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

static ERTS_INLINE int
send_error_term_to_logger(Eterm gleader, char *buf, int len, Eterm args)
{
    return do_send_term_to_logger(am_error, gleader, buf, len, args);
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
erts_send_error_term_to_logger(Eterm gleader, erts_dsprintf_buf_t *dsbufp, Eterm args)
{
    int res;
    res = send_error_term_to_logger(gleader, dsbufp->str, dsbufp->str_len, args);
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
    if (is_same(a, b)) goto pop_next;
tailrecur_ne:

    switch (primary_tag(a)) {
    case TAG_PRIMARY_LIST:
	if (is_list(b)) {
	    Eterm* aval = list_val(a);
	    Eterm* bval = list_val(b);
	    while (1) {
		Eterm atmp = CAR(aval);
		Eterm btmp = CAR(bval);
		if (!is_same(atmp,btmp)) {
		    WSTACK_PUSH2(stack,(UWord) CDR(bval),(UWord) CDR(aval));
		    a = atmp;
		    b = btmp;
		    goto tailrecur_ne;
		}
		atmp = CDR(aval);
		btmp = CDR(bval);
		if (is_same(atmp,btmp)) {
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

		    if (!is_binary(b)) {
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

		    if (!is_fun(b))
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

		if(!is_external(b))
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
		ExternalThing* athing;
		ExternalThing* bthing;

		if(!is_external_ref(b))
		    goto not_equal;

		athing = external_thing_ptr(a);
		bthing = external_thing_ptr(b);

		if(athing->node != bthing->node)
		    goto not_equal;

		anum = external_thing_ref_numbers(athing);
		bnum = external_thing_ref_numbers(bthing);
		alen = external_thing_ref_no_of_numbers(athing);
		blen = external_thing_ref_no_of_numbers(bthing);

		goto ref_common;
	    case REF_SUBTAG:
		    if (!is_internal_ref(b))
			goto not_equal;

		    {
			RefThing* athing = ref_thing_ptr(a);
			RefThing* bthing = ref_thing_ptr(b);
			alen = internal_thing_ref_no_of_numbers(athing);
			blen = internal_thing_ref_no_of_numbers(bthing);
			anum = internal_thing_ref_numbers(athing);
			bnum = internal_thing_ref_numbers(bthing);
		    }

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

		    if (!is_big(b))
			goto not_equal;
		    aa = big_val(a);
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
	    case MAP_SUBTAG:
                if (is_flatmap(a)) {
		    aa = flatmap_val(a);
		    if (!is_boxed(b) || *boxed_val(b) != *aa)
			goto not_equal;
		    bb = flatmap_val(b);
		    sz = flatmap_get_size((flatmap_t*)aa);

		    if (sz != flatmap_get_size((flatmap_t*)bb)) goto not_equal;
		    if (sz == 0) goto pop_next;

		    aa += 2;
		    bb += 2;
		    sz += 1; /* increment for tuple-keys */
		    goto term_array;

                } else {
		    if (!is_boxed(b) || *boxed_val(b) != hdr)
			goto not_equal;

		    aa = hashmap_val(a) + 1;
		    bb = hashmap_val(b) + 1;
		    switch (hdr & _HEADER_MAP_SUBTAG_MASK) {
		    case HAMT_SUBTAG_HEAD_ARRAY:
			aa++; bb++;
			sz = 16;
			break;
		    case HAMT_SUBTAG_HEAD_BITMAP:
			aa++; bb++;
		    case HAMT_SUBTAG_NODE_BITMAP:
			sz = hashmap_bitcount(MAP_HEADER_VAL(hdr));
			ASSERT(sz > 0 && sz < 17);
			break;
		    default:
			erts_exit(ERTS_ERROR_EXIT, "Unknown hashmap subsubtag\n");
		    }
		    goto term_array;
		}
	    default:
		ASSERT(!"Unknown boxed subtab in EQ");
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
	    if (!is_same(*ap,*bp)) break;
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
 *   tuples < maps < [] < conses < binaries.
 *
 * Note that characters are currently not implemented.
 *
 */


#define float_comp(x,y)    (((x)<(y)) ? -1 : (((x)==(y)) ? 0 : 1))

int erts_cmp_atoms(Eterm a, Eterm b)
{
    Atom *aa = atom_tab(atom_val(a));
    Atom *bb = atom_tab(atom_val(b));
    int diff = aa->ord0 - bb->ord0;
    if (diff)
	return diff;
    return cmpbytes(aa->name+3, aa->len-3,
		    bb->name+3, bb->len-3);
}

/* cmp(Eterm a, Eterm b)
 *  For compatibility with HiPE - arith-based compare.
 */
Sint cmp(Eterm a, Eterm b)
{
    return erts_cmp(a, b, 0, 0);
}

Sint erts_cmp_compound(Eterm a, Eterm b, int exact, int eq_only);

Sint erts_cmp(Eterm a, Eterm b, int exact, int eq_only)
{
    if (is_atom(a) && is_atom(b)) {
        return erts_cmp_atoms(a, b);
    } else if (is_both_small(a, b)) {
        return (signed_val(a) - signed_val(b));
    } else if (is_float(a) && is_float(b)) {
        FloatDef af, bf;
        GET_DOUBLE(a, af);
        GET_DOUBLE(b, bf);
        return float_comp(af.fd, bf.fd);
    }
    return erts_cmp_compound(a,b,exact,eq_only);
}


/* erts_cmp(Eterm a, Eterm b, int exact)
 * exact = 1 -> term-based compare
 * exact = 0 -> arith-based compare
 */
Sint erts_cmp_compound(Eterm a, Eterm b, int exact, int eq_only)
{
#define PSTACK_TYPE struct erts_cmp_hashmap_state
    struct erts_cmp_hashmap_state {
        Sint wstack_rollback;
        int was_exact;
        Eterm *ap;
        Eterm *bp;
        Eterm min_key;
        Sint cmp_res;   /* result so far -1,0,+1 */
    };
    PSTACK_DECLARE(hmap_stack, 1);
    WSTACK_DECLARE(stack);
    WSTACK_DECLARE(b_stack); /* only used by hashmaps */
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

/* The WSTACK contains naked Eterms and Operations marked with header-tags */
#define OP_BITS 4
#define OP_MASK 0xF
#define TERM_ARRAY_OP                 0
#define SWITCH_EXACT_OFF_OP           1
#define HASHMAP_PHASE1_ARE_KEYS_EQUAL 2
#define HASHMAP_PHASE1_IS_MIN_KEY     3
#define HASHMAP_PHASE1_CMP_VALUES     4
#define HASHMAP_PHASE2_ARE_KEYS_EQUAL 5
#define HASHMAP_PHASE2_IS_MIN_KEY_A   6
#define HASHMAP_PHASE2_IS_MIN_KEY_B   7


#define OP_WORD(OP)  (((OP)  << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER)
#define TERM_ARRAY_OP_WORD(SZ) OP_WORD(((SZ) << OP_BITS) | TERM_ARRAY_OP)

#define GET_OP(WORD) (ASSERT(is_header(WORD)), ((WORD) >> _TAG_PRIMARY_SIZE) & OP_MASK)
#define GET_OP_ARG(WORD) (ASSERT(is_header(WORD)), ((WORD) >> (OP_BITS + _TAG_PRIMARY_SIZE)))


#define RETURN_NEQ(cmp) { j=(cmp); ASSERT(j != 0); goto not_equal; }
#define ON_CMP_GOTO(cmp) if ((j=(cmp)) == 0) goto pop_next; else goto not_equal

#undef  CMP_NODES
#define CMP_NODES(AN, BN)						\
    do {								\
	if((AN) != (BN)) {						\
            if((AN)->sysname != (BN)->sysname)				\
                RETURN_NEQ(erts_cmp_atoms((AN)->sysname, (BN)->sysname));	\
	    ASSERT((AN)->creation != (BN)->creation);			\
	    RETURN_NEQ(((AN)->creation < (BN)->creation) ? -1 : 1);	\
	}								\
    } while (0)


bodyrecur:
    j = 0;
tailrecur:
    if (is_same(a,b)) {	/* Equal values or pointers. */
	goto pop_next;
    }
tailrecur_ne:

    /* deal with majority (?) cases by brute-force */
    if (is_atom(a)) {
	if (is_atom(b)) {
	    ON_CMP_GOTO(erts_cmp_atoms(a, b));
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
	    if (!is_same(atmp,btmp)) {
		WSTACK_PUSH2(stack,(UWord) CDR(bb),(UWord) CDR(aa));
		a = atmp;
		b = btmp;
		goto tailrecur_ne;
	    }
	    atmp = CDR(aa);
	    btmp = CDR(bb);
	    if (is_same(atmp,btmp)) {
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
		if (!is_tuple(b)) {
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
            case (_TAG_HEADER_MAP >> _TAG_PRIMARY_SIZE) :
		{
                    struct erts_cmp_hashmap_state* sp;
                    if (is_flatmap_header(ahdr)) {
                        if (!is_flatmap(b)) {
                            if (is_hashmap(b)) {
                                aa = (Eterm *)flatmap_val(a);
                                i = flatmap_get_size((flatmap_t*)aa) - hashmap_size(b);
                                ASSERT(i != 0);
                                RETURN_NEQ(i);
                            }
                            a_tag = MAP_DEF;
                            goto mixed_types;
                        }
                        aa = (Eterm *)flatmap_val(a);
                        bb = (Eterm *)flatmap_val(b);

                        i = flatmap_get_size((flatmap_t*)aa);
                        if (i != flatmap_get_size((flatmap_t*)bb)) {
                            RETURN_NEQ((int)(i - flatmap_get_size((flatmap_t*)bb)));
                        }
                        if (i == 0) {
                            goto pop_next;
                        }
                        aa += 2;
                        bb += 2;
                        if (exact) {
                            i  += 1; /* increment for tuple-keys */
                            goto term_array;
                        }
                        else {
                            /* Value array */
                            WSTACK_PUSH3(stack,(UWord)(bb+1),(UWord)(aa+1),TERM_ARRAY_OP_WORD(i));
                            /* Switch back from 'exact' key compare */
                            WSTACK_PUSH(stack,OP_WORD(SWITCH_EXACT_OFF_OP));
                            /* Now do 'exact' compare of key tuples */
                            a = *aa;
                            b = *bb;
                            exact = 1;
                            goto bodyrecur;
                        }
                    }
		    if (!is_hashmap(b)) {
                        if (is_flatmap(b)) {
                            bb = (Eterm *)flatmap_val(b);
                            i = hashmap_size(a) - flatmap_get_size((flatmap_t*)bb);
                            ASSERT(i != 0);
                            RETURN_NEQ(i);
                        }
			a_tag = MAP_DEF;
			goto mixed_types;
		    }
		    i = hashmap_size(a) - hashmap_size(b);
		    if (i) {
			RETURN_NEQ(i);
		    }
                    if (hashmap_size(a) == 0) {
                        goto pop_next;
                    }

                /* Hashmap compare strategy:
                   Phase 1. While keys are identical
                     Do synchronous stepping through leafs of both trees in hash
                     order. Maintain value compare result of minimal key.

                   Phase 2. If key diff was found in phase 1
                     Ignore values from now on.
                     Continue iterate trees by always advancing the one
                     lagging behind hash-wise. Identical keys are skipped.
                     A minimal key can only be candidate as tie-breaker if we
                     have passed that hash value in the other tree (which means
                     the key did not exist in the other tree).
                */

                    sp = PSTACK_PUSH(hmap_stack);
                    hashmap_iterator_init(&stack, a, 0);
                    hashmap_iterator_init(&b_stack, b, 0);
                    sp->ap = hashmap_iterator_next(&stack);
                    sp->bp = hashmap_iterator_next(&b_stack);
                    sp->cmp_res = 0;
                    ASSERT(sp->ap && sp->bp);

                    a = CAR(sp->ap);
                    b = CAR(sp->bp);
                    sp->was_exact = exact;
                    exact = 1;
                    WSTACK_PUSH(stack, OP_WORD(HASHMAP_PHASE1_ARE_KEYS_EQUAL));
                    sp->wstack_rollback = WSTACK_COUNT(stack);
                    goto bodyrecur;
		}
	    case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		if (!is_float(b)) {
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
		if (!is_big(b)) {
		    a_tag = BIG_DEF;
		    goto mixed_types;
		}
		ON_CMP_GOTO(big_comp(a, b));
	    case (_TAG_HEADER_EXPORT >> _TAG_PRIMARY_SIZE):
		if (!is_export(b)) {
		    a_tag = EXPORT_DEF;
		    goto mixed_types;
		} else {
		    Export* a_exp = *((Export **) (export_val(a) + 1));
		    Export* b_exp = *((Export **) (export_val(b) + 1));

		    if ((j = erts_cmp_atoms(a_exp->code[0], b_exp->code[0])) != 0) {
			RETURN_NEQ(j);
		    }
		    if ((j = erts_cmp_atoms(a_exp->code[1], b_exp->code[1])) != 0) {
			RETURN_NEQ(j);
		    }
		    ON_CMP_GOTO((Sint) a_exp->code[2] - (Sint) b_exp->code[2]);
		}
		break;
	    case (_TAG_HEADER_FUN >> _TAG_PRIMARY_SIZE):
		if (!is_fun(b)) {
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
		    RefThing* bthing = ref_thing_ptr(b);
		    bnode = erts_this_node;
		    bnum = internal_thing_ref_numbers(bthing);
		    blen = internal_thing_ref_no_of_numbers(bthing);
		} else if(is_external_ref(b)) {
		    ExternalThing* bthing = external_thing_ptr(b);
		    bnode = bthing->node;
		    bnum = external_thing_ref_numbers(bthing);
		    blen = external_thing_ref_no_of_numbers(bthing);
		} else {
		    a_tag = REF_DEF;
		    goto mixed_types;
		}
		{
		    RefThing* athing = ref_thing_ptr(a);
		    anode = erts_this_node;
		    anum = internal_thing_ref_numbers(athing);
		    alen = internal_thing_ref_no_of_numbers(athing);
		}

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
		    RefThing* bthing = ref_thing_ptr(b);
		    bnode = erts_this_node;
		    bnum = internal_thing_ref_numbers(bthing);
		    blen = internal_thing_ref_no_of_numbers(bthing);
		} else if (is_external_ref(b)) {
		    ExternalThing* bthing = external_thing_ptr(b);
		    bnode = bthing->node;
		    bnum = external_thing_ref_numbers(bthing);
		    blen = external_thing_ref_no_of_numbers(bthing);
		} else {
		    a_tag = EXTERNAL_REF_DEF;
		    goto mixed_types;
		}
		{
		    ExternalThing* athing = external_thing_ptr(a);
		    anode = athing->node;
		    anum = external_thing_ref_numbers(athing);
		    alen = external_thing_ref_no_of_numbers(athing);
		}
		goto ref_common;
	    default:
		/* Must be a binary */
		ASSERT(is_binary(a));
		if (!is_binary(b)) {
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

    {
	FloatDef f1, f2;
	Eterm big;
	Eterm aw = a;
	Eterm bw = b;
#define MAX_LOSSLESS_FLOAT ((double)((1LL << 53) - 2))
#define MIN_LOSSLESS_FLOAT ((double)(((1LL << 53) - 2)*-1))
#define BIG_ARITY_FLOAT_MAX (1024 / D_EXP) /* arity of max float as a bignum */
	Eterm big_buf[BIG_NEED_SIZE(BIG_ARITY_FLOAT_MAX)];

	b_tag = tag_val_def(bw);

	switch(_NUMBER_CODE(a_tag, b_tag)) {
	case SMALL_BIG:
	    j = big_sign(bw) ? 1 : -1;
	    break;
	case BIG_SMALL:
	    j = big_sign(aw) ? -1 : 1;
	    break;
	case SMALL_FLOAT:
	    if (exact) goto exact_fall_through;
	    GET_DOUBLE(bw, f2);
	    if (f2.fd < MAX_LOSSLESS_FLOAT && f2.fd > MIN_LOSSLESS_FLOAT) {
		/* Float is within the no loss limit */
		f1.fd = signed_val(aw);
		j = float_comp(f1.fd, f2.fd);
	    }
#if ERTS_SIZEOF_ETERM == 8
	    else if (f2.fd > (double) (MAX_SMALL + 1)) {
		/* Float is a positive bignum, i.e. bigger */
		j = -1;
	    } else if (f2.fd < (double) (MIN_SMALL - 1)) {
		/* Float is a negative bignum, i.e. smaller */
		j = 1;
	    } else {
		/* Float is a Sint but less precise */
		j = signed_val(aw) - (Sint) f2.fd;
	    }
#else
	    else {
		/* If float is positive it is bigger than small */
		j = (f2.fd > 0.0) ? -1 : 1;
	    }
#endif /* ERTS_SIZEOF_ETERM == 8 */
	    break;
        case FLOAT_BIG:
	    if (exact) goto exact_fall_through;
	{
	    Wterm tmp = aw;
	    aw = bw;
	    bw = tmp;
	}/* fall through */
	case BIG_FLOAT:
	    if (exact) goto exact_fall_through;
	    GET_DOUBLE(bw, f2);
	    if ((f2.fd < (double) (MAX_SMALL + 1))
		    && (f2.fd > (double) (MIN_SMALL - 1))) {
		/* Float is a Sint */
		j = big_sign(aw) ? -1 : 1;
	    } else if (big_arity(aw) > BIG_ARITY_FLOAT_MAX
		       || pow(2.0,(big_arity(aw)-1)*D_EXP) > fabs(f2.fd)) {
		/* If bignum size shows that it is bigger than the abs float */
		j = big_sign(aw) ? -1 : 1;
	    } else if (big_arity(aw) < BIG_ARITY_FLOAT_MAX
		       && (pow(2.0,(big_arity(aw))*D_EXP)-1.0) < fabs(f2.fd)) {
		/* If bignum size shows that it is smaller than the abs float */
		j = f2.fd < 0 ? 1 : -1;
	    } else if (f2.fd < MAX_LOSSLESS_FLOAT && f2.fd > MIN_LOSSLESS_FLOAT) {
		/* Float is within the no loss limit */
		if (big_to_double(aw, &f1.fd) < 0) {
		    j = big_sign(aw) ? -1 : 1;
		} else {
		    j = float_comp(f1.fd, f2.fd);
		}
	    } else {
		big = double_to_big(f2.fd, big_buf, sizeof(big_buf)/sizeof(Eterm));
		j = big_comp(aw, big);
	    }
	    if (_NUMBER_CODE(a_tag, b_tag) == FLOAT_BIG) {
		j = -j;
	    }
	    break;
	case FLOAT_SMALL:
	    if (exact) goto exact_fall_through;
	    GET_DOUBLE(aw, f1);
	    if (f1.fd < MAX_LOSSLESS_FLOAT && f1.fd > MIN_LOSSLESS_FLOAT) {
		/* Float is within the no loss limit */
		f2.fd = signed_val(bw);
		j = float_comp(f1.fd, f2.fd);
	    }
#if ERTS_SIZEOF_ETERM == 8
	    else if (f1.fd > (double) (MAX_SMALL + 1)) {
		/* Float is a positive bignum, i.e. bigger */
		j = 1;
	    } else if (f1.fd < (double) (MIN_SMALL - 1)) {
		/* Float is a negative bignum, i.e. smaller */
		j = -1;
	    } else {
		/* Float is a Sint but less precise it */
		j = (Sint) f1.fd - signed_val(bw);
	    }
#else
	    else {
		/* If float is positive it is bigger than small */
		j = (f1.fd > 0.0) ? 1 : -1;
	    }
#endif /* ERTS_SIZEOF_ETERM == 8 */
	    break;
exact_fall_through:
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
	if (!is_same(a, b)) {
	    if (is_atom(a) && is_atom(b)) {
		if ((j = erts_cmp_atoms(a, b)) != 0) {
		    goto not_equal;
		}
	    } else if (is_both_small(a, b)) {
		if ((j = signed_val(a)-signed_val(b)) != 0) {
		    goto not_equal;
		}
	    } else {
		WSTACK_PUSH3(stack, (UWord)bb, (UWord)aa, TERM_ARRAY_OP_WORD(i));
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
        struct erts_cmp_hashmap_state* sp;
	if (primary_tag((Eterm) something) == TAG_PRIMARY_HEADER) { /* an operation */
	    switch (GET_OP(something)) {
	    case TERM_ARRAY_OP:
		i = GET_OP_ARG(something);
		aa = (Eterm*)WSTACK_POP(stack);
		bb = (Eterm*) WSTACK_POP(stack);
		goto term_array;

	    case SWITCH_EXACT_OFF_OP:
		/* Done with exact compare of map keys, switch back */
		ASSERT(exact);
		exact = 0;
		goto pop_next;

            case HASHMAP_PHASE1_ARE_KEYS_EQUAL: {
                sp = PSTACK_TOP(hmap_stack);
                if (j) {
                    /* Key diff found, enter phase 2 */
                    if (hashmap_key_hash_cmp(sp->ap, sp->bp) < 0) {
                        sp->min_key = CAR(sp->ap);
                        sp->cmp_res = -1;
                        sp->ap = hashmap_iterator_next(&stack);
                    }
                    else {
                        sp->min_key = CAR(sp->bp);
                        sp->cmp_res = 1;
                        sp->bp = hashmap_iterator_next(&b_stack);
                    }
                    exact = 1; /* only exact key compares in phase 2 */
                    goto case_HASHMAP_PHASE2_LOOP;
                }

                /* No key diff found so far, compare values if min key */

                if (sp->cmp_res) {
                    a = CAR(sp->ap);
                    b = sp->min_key;
                    exact = 1;
                    WSTACK_PUSH(stack, OP_WORD(HASHMAP_PHASE1_IS_MIN_KEY));
                    sp->wstack_rollback = WSTACK_COUNT(stack);
                    goto bodyrecur;
                }
                /* no min key-value found yet */
                a = CDR(sp->ap);
                b = CDR(sp->bp);
                exact = sp->was_exact;
                WSTACK_PUSH(stack, OP_WORD(HASHMAP_PHASE1_CMP_VALUES));
                sp->wstack_rollback = WSTACK_COUNT(stack);
                goto bodyrecur;
            }
            case HASHMAP_PHASE1_IS_MIN_KEY:
                sp = PSTACK_TOP(hmap_stack);
                if (j < 0) {
                    a = CDR(sp->ap);
                    b = CDR(sp->bp);
                    exact = sp->was_exact;
                    WSTACK_PUSH(stack, OP_WORD(HASHMAP_PHASE1_CMP_VALUES));
                    sp->wstack_rollback = WSTACK_COUNT(stack);
                    goto bodyrecur;
                }
                goto case_HASHMAP_PHASE1_LOOP;

            case HASHMAP_PHASE1_CMP_VALUES:
                sp = PSTACK_TOP(hmap_stack);
                if (j) {
                    sp->cmp_res = j;
                    sp->min_key = CAR(sp->ap);
                }
            case_HASHMAP_PHASE1_LOOP:
                sp->ap = hashmap_iterator_next(&stack);
                sp->bp = hashmap_iterator_next(&b_stack);
                if (!sp->ap) {
                    /* end of maps with identical keys */
                    ASSERT(!sp->bp);
                    j = sp->cmp_res;
                    exact = sp->was_exact;
                    (void) PSTACK_POP(hmap_stack);
                    ON_CMP_GOTO(j);
                }
                a = CAR(sp->ap);
                b = CAR(sp->bp);
                exact = 1;
                WSTACK_PUSH(stack, OP_WORD(HASHMAP_PHASE1_ARE_KEYS_EQUAL));
                sp->wstack_rollback = WSTACK_COUNT(stack);
                goto bodyrecur;

            case_HASHMAP_PHASE2_LOOP:
                if (sp->ap && sp->bp) {
                    a = CAR(sp->ap);
                    b = CAR(sp->bp);
                    ASSERT(exact);
                    WSTACK_PUSH(stack, OP_WORD(HASHMAP_PHASE2_ARE_KEYS_EQUAL));
                    sp->wstack_rollback = WSTACK_COUNT(stack);
                    goto bodyrecur;
                }
                goto case_HASHMAP_PHASE2_NEXT_STEP;

            case HASHMAP_PHASE2_ARE_KEYS_EQUAL:
                sp = PSTACK_TOP(hmap_stack);
                if (j == 0) {
                    /* keys are equal, skip them */
                    sp->ap = hashmap_iterator_next(&stack);
                    sp->bp = hashmap_iterator_next(&b_stack);
                    goto case_HASHMAP_PHASE2_LOOP;
                }
                /* fall through */
            case_HASHMAP_PHASE2_NEXT_STEP:
                if (sp->ap || sp->bp) {
                    if (hashmap_key_hash_cmp(sp->ap, sp->bp) < 0) {
                        ASSERT(sp->ap);
                        a = CAR(sp->ap);
                        b = sp->min_key;
                        ASSERT(exact);
                        WSTACK_PUSH(stack, OP_WORD(HASHMAP_PHASE2_IS_MIN_KEY_A));
                    }
                    else { /* hash_cmp > 0 */
                        ASSERT(sp->bp);
                        a = CAR(sp->bp);
                        b = sp->min_key;
                        ASSERT(exact);
                        WSTACK_PUSH(stack, OP_WORD(HASHMAP_PHASE2_IS_MIN_KEY_B));
                    }
                    sp->wstack_rollback = WSTACK_COUNT(stack);
                    goto bodyrecur;
                }
                /* End of both maps */
                j = sp->cmp_res;
                exact = sp->was_exact;
                (void) PSTACK_POP(hmap_stack);
                ON_CMP_GOTO(j);

            case HASHMAP_PHASE2_IS_MIN_KEY_A:
                sp = PSTACK_TOP(hmap_stack);
                if (j < 0) {
                    sp->min_key = CAR(sp->ap);
                    sp->cmp_res = -1;
                }
                sp->ap = hashmap_iterator_next(&stack);
                goto case_HASHMAP_PHASE2_LOOP;

            case HASHMAP_PHASE2_IS_MIN_KEY_B:
                sp = PSTACK_TOP(hmap_stack);
                if (j < 0) {
                    sp->min_key = CAR(sp->bp);
                    sp->cmp_res = 1;
                }
                sp->bp = hashmap_iterator_next(&b_stack);
                goto case_HASHMAP_PHASE2_LOOP;

            default:
                ASSERT(!"Invalid cmp op");
            } /* switch */
	}
	a = (Eterm) something;
	b = (Eterm) WSTACK_POP(stack);
	goto tailrecur;
    }

    ASSERT(PSTACK_IS_EMPTY(hmap_stack));
    PSTACK_DESTROY(hmap_stack);
    WSTACK_DESTROY(stack);
    WSTACK_DESTROY(b_stack);
    return 0;

not_equal:
    if (!PSTACK_IS_EMPTY(hmap_stack) && !eq_only) {
        WSTACK_ROLLBACK(stack, PSTACK_TOP(hmap_stack)->wstack_rollback);
        goto pop_next;
    }
    PSTACK_DESTROY(hmap_stack);
    WSTACK_DESTROY(stack);
    WSTACK_DESTROY(b_stack);
    return j;

#undef CMP_NODES
}


Eterm
store_external_or_ref_(Uint **hpp, ErlOffHeap* oh, Eterm ns)
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

	((struct erl_off_heap_header*) to_hp)->next = oh->first;
	oh->first = (struct erl_off_heap_header*) to_hp;

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
    return store_external_or_ref_(&hp, &MSO(proc), ns);
}

void bin_write(int to, void *to_arg, byte* buf, size_t sz)
{
    size_t i;

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
 * return number of chars in list
 * or -1 for type error
 * or -2 for not enough buffer space (buffer contains truncated result)
 */
Sint
intlist_to_buf(Eterm list, char *buf, Sint len)
{
    Eterm* listptr;
    Sint sz = 0;

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
    return -2;			/* not enough space */
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
buf_to_intlist(Eterm** hpp, const char *buf, size_t len, Eterm tail)
{
    Eterm* hp = *hpp;
    size_t i = len;

    while(i != 0) {
	--i;
	tail = CONS(hp, make_small((Uint)(byte)buf[i]), tail);
	hp += 2;
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
**        ERTS_IOLIST_TO_BUF_OVERFLOW on overflow
**        ERTS_IOLIST_TO_BUF_TYPE_ERROR on type error (including that result would not be a whole number of bytes)
**
** Note! 
** Do not detect indata errors in this fiunction that are not detected by erts_iolist_size!
**
** A caller should be able to rely on a successful return from erts_iolist_to_buf
** if erts_iolist_size is previously successfully called and erts_iolist_to_buf 
** is called with a buffer at least as large as the value given by erts_iolist_size.
** 
*/

typedef enum {
    ERTS_IL2B_BCOPY_OK,
    ERTS_IL2B_BCOPY_YIELD,
    ERTS_IL2B_BCOPY_OVERFLOW,
    ERTS_IL2B_BCOPY_TYPE_ERROR
} ErtsIL2BBCopyRes;

static ErtsIL2BBCopyRes
iolist_to_buf_bcopy(ErtsIOList2BufState *state, Eterm obj, int *yield_countp);

static ERTS_INLINE ErlDrvSizeT
iolist_to_buf(const int yield_support,
	      ErtsIOList2BufState *state,
	      Eterm obj,
	      char* buf,
	      ErlDrvSizeT alloced_len)
{
#undef IOLIST_TO_BUF_BCOPY
#define IOLIST_TO_BUF_BCOPY(CONSP)					\
do {									\
    size_t size = binary_size(obj);					\
    if (size > 0) {							\
	Uint bitsize;							\
	byte* bptr;							\
	Uint bitoffs;							\
	Uint num_bits;							\
	if (yield_support) {						\
	    size_t max_size = ERTS_IOLIST_TO_BUF_BYTES_PER_YIELD_COUNT;	\
	    if (yield_count > 0)					\
		max_size *= yield_count+1;				\
	    if (size > max_size) {					\
		state->objp = CONSP;					\
		goto L_bcopy_yield;					\
	    }								\
	    if (size >= ERTS_IOLIST_TO_BUF_BYTES_PER_YIELD_COUNT) {	\
		int cost = (int) size;					\
		cost /= ERTS_IOLIST_TO_BUF_BYTES_PER_YIELD_COUNT;	\
		yield_count -= cost;					\
	    }								\
	}								\
	if (len < size)							\
	    goto L_overflow;						\
	ERTS_GET_BINARY_BYTES(obj, bptr, bitoffs, bitsize);		\
	if (bitsize != 0)						\
	    goto L_type_error;						\
	num_bits = 8*size;						\
	copy_binary_to_buffer(buf, 0, bptr, bitoffs, num_bits);		\
	buf += size;							\
	len -= size;							\
    }									\
} while (0)

    ErlDrvSizeT res, len;
    Eterm* objp = NULL;
    int init_yield_count;
    int yield_count;
    DECLARE_ESTACK(s);

    len = (ErlDrvSizeT) alloced_len;

    if (!yield_support) {
	yield_count = init_yield_count = 0; /* Shut up faulty warning... >:-( */
	goto L_again;
    }
    else {

	if (state->iolist.reds_left <= 0)
	    return ERTS_IOLIST_TO_BUF_YIELD;

	ESTACK_CHANGE_ALLOCATOR(s, ERTS_ALC_T_SAVED_ESTACK);
	init_yield_count = (ERTS_IOLIST_TO_BUF_YIELD_COUNT_PER_RED
			   * state->iolist.reds_left);
	yield_count = init_yield_count;

	if (!state->iolist.estack.start)
	    goto L_again;
	else {
	    int chk_stack;
	    /* Restart; restore state... */
	    ESTACK_RESTORE(s, &state->iolist.estack);

	    if (!state->bcopy.bptr)
		chk_stack = 0;
	    else {
		chk_stack = 1;
		switch (iolist_to_buf_bcopy(state, THE_NON_VALUE, &yield_count)) {
		case ERTS_IL2B_BCOPY_OK:
		    break;
		case ERTS_IL2B_BCOPY_YIELD:
		    BUMP_ALL_REDS(state->iolist.c_p);
		    state->iolist.reds_left = 0;
		    ESTACK_SAVE(s, &state->iolist.estack);
		    return ERTS_IOLIST_TO_BUF_YIELD;
		case ERTS_IL2B_BCOPY_OVERFLOW:
		    goto L_overflow;
		case ERTS_IL2B_BCOPY_TYPE_ERROR:
		    goto L_type_error;
		}
	    }

	    obj = state->iolist.obj;
	    buf = state->buf;
	    len = state->len;
	    objp = state->objp;
	    state->objp = NULL;
	    if (objp)
		goto L_tail;
	    if (!chk_stack)
		goto L_again;
	    /* check stack */
	}
    }

    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_again:
	if (is_list(obj)) {
	    while (1) { /* Tail loop */
		while (1) { /* Head loop */
		    if (yield_support && --yield_count <= 0)
			goto L_yield;
		    objp = list_val(obj);
		    obj = CAR(objp);
		    if (is_byte(obj)) {
			if (len == 0) {
			    goto L_overflow;
			}
			*buf++ = unsigned_val(obj);
			len--;
		    } else if (is_binary(obj)) {
			IOLIST_TO_BUF_BCOPY(objp);
		    } else if (is_list(obj)) {
			ESTACK_PUSH(s, CDR(objp));
			continue; /* Head loop */
		    } else if (is_not_nil(obj)) {
			goto L_type_error;
		    }
		    break;
		}

	    L_tail:

		obj = CDR(objp);

		if (is_list(obj)) {
		    continue; /* Tail loop */
		} else if (is_binary(obj)) {
		    IOLIST_TO_BUF_BCOPY(NULL);
		} else if (is_not_nil(obj)) {
		    goto L_type_error;
		}
		break;
	    }
	} else if (is_binary(obj)) {
	    IOLIST_TO_BUF_BCOPY(NULL);
	} else if (is_not_nil(obj)) {
	    goto L_type_error;
	} else if (yield_support && --yield_count <= 0)
	    goto L_yield;
    }

    res = len;

 L_return:

    DESTROY_ESTACK(s);

    if (yield_support) {
	int reds;
	CLEAR_SAVED_ESTACK(&state->iolist.estack);
	reds = ((init_yield_count - yield_count - 1)
		/ ERTS_IOLIST_TO_BUF_YIELD_COUNT_PER_RED) + 1;
	BUMP_REDS(state->iolist.c_p, reds);
	state->iolist.reds_left -= reds;
	if (state->iolist.reds_left < 0)
	    state->iolist.reds_left = 0;
    }


    return res;

 L_type_error:
    res = ERTS_IOLIST_TO_BUF_TYPE_ERROR;
    goto L_return;

 L_overflow:
    res = ERTS_IOLIST_TO_BUF_OVERFLOW;
    goto L_return;

 L_bcopy_yield:

    state->buf = buf;
    state->len = len;

    switch (iolist_to_buf_bcopy(state, obj, &yield_count)) {
    case ERTS_IL2B_BCOPY_OK:
	ERTS_INTERNAL_ERROR("Missing yield");
    case ERTS_IL2B_BCOPY_YIELD:
	BUMP_ALL_REDS(state->iolist.c_p);
	state->iolist.reds_left = 0;
	ESTACK_SAVE(s, &state->iolist.estack);
	return ERTS_IOLIST_TO_BUF_YIELD;
    case ERTS_IL2B_BCOPY_OVERFLOW:
	goto L_overflow;
    case ERTS_IL2B_BCOPY_TYPE_ERROR:
	goto L_type_error;
    }

 L_yield:

    BUMP_ALL_REDS(state->iolist.c_p);
    state->iolist.reds_left = 0;
    state->iolist.obj = obj;
    state->buf = buf;
    state->len = len;
    ESTACK_SAVE(s, &state->iolist.estack);
    return ERTS_IOLIST_TO_BUF_YIELD;

#undef IOLIST_TO_BUF_BCOPY
}

static ErtsIL2BBCopyRes
iolist_to_buf_bcopy(ErtsIOList2BufState *state, Eterm obj, int *yield_countp)
{
    ErtsIL2BBCopyRes res;
    char *buf = state->buf;
    ErlDrvSizeT len = state->len;
    byte* bptr;
    size_t size;
    size_t max_size;
    Uint bitoffs;
    Uint num_bits;
    int yield_count = *yield_countp;

    if (state->bcopy.bptr) {
	bptr = state->bcopy.bptr;
	size = state->bcopy.size;
	bitoffs = state->bcopy.bitoffs;
	state->bcopy.bptr = NULL;
    }
    else {
	Uint bitsize;

	ASSERT(is_binary(obj));

	size = binary_size(obj);
	if (size <= 0)
	    return ERTS_IL2B_BCOPY_OK;

	if (len < size)
	    return ERTS_IL2B_BCOPY_OVERFLOW;

	ERTS_GET_BINARY_BYTES(obj, bptr, bitoffs, bitsize);
	if (bitsize != 0)
	    return ERTS_IL2B_BCOPY_TYPE_ERROR;
    }

    ASSERT(size > 0);
    max_size = (size_t) ERTS_IOLIST_TO_BUF_BYTES_PER_YIELD_COUNT;
    if (yield_count > 0)
	max_size *= (size_t) (yield_count+1);

    if (size <= max_size) {
	if (size >= ERTS_IOLIST_TO_BUF_BYTES_PER_YIELD_COUNT) {
	    int cost = (int) size;
	    cost /= ERTS_IOLIST_TO_BUF_BYTES_PER_YIELD_COUNT;
	    yield_count -= cost;
	}
	res = ERTS_IL2B_BCOPY_OK;
    }
    else {
	ASSERT(0 < max_size && max_size < size);
	yield_count = 0;
	state->bcopy.bptr = bptr + max_size;
	state->bcopy.bitoffs = bitoffs;
	state->bcopy.size = size - max_size;
	size = max_size;
	res = ERTS_IL2B_BCOPY_YIELD;
    }

    num_bits = 8*size;
    copy_binary_to_buffer(buf, 0, bptr, bitoffs, num_bits);
    state->buf += size;
    state->len -= size;
    *yield_countp = yield_count;

    return res;
}

ErlDrvSizeT erts_iolist_to_buf_yielding(ErtsIOList2BufState *state)
{
    return iolist_to_buf(1, state, state->iolist.obj, state->buf, state->len);
}

ErlDrvSizeT erts_iolist_to_buf(Eterm obj, char* buf, ErlDrvSizeT alloced_len)
{
    return iolist_to_buf(0, NULL, obj, buf, alloced_len);
}

/*
 * Return 0 if successful, and non-zero if unsuccessful.
 *
 * It is vital that if erts_iolist_to_buf would return an error for
 * any type of term data, this function should do so as well.
 * Any input term error detected in erts_iolist_to_buf should also
 * be detected in this function!
 */

static ERTS_INLINE int
iolist_size(const int yield_support, ErtsIOListState *state, Eterm obj, ErlDrvSizeT* sizep)
{
    int res, init_yield_count, yield_count;
    Eterm* objp;
    Uint size = (Uint) *sizep;
    DECLARE_ESTACK(s);

    if (!yield_support)
	yield_count = init_yield_count = 0; /* Shut up faulty warning... >:-( */
    else {
	if (state->reds_left <= 0)
	    return ERTS_IOLIST_YIELD;
	ESTACK_CHANGE_ALLOCATOR(s, ERTS_ALC_T_SAVED_ESTACK);
	init_yield_count = ERTS_IOLIST_SIZE_YIELDS_COUNT_PER_RED;
	init_yield_count *= state->reds_left;
	yield_count = init_yield_count;
	if (state->estack.start) {
	    /* Restart; restore state... */
	    ESTACK_RESTORE(s, &state->estack);
	    size = (Uint) state->size;
	    obj = state->obj;
	}
    }

    goto L_again;

#define SAFE_ADD(Var, Val)			\
    do {					\
        Uint valvar = (Val);			\
	Var += valvar;				\
	if (Var < valvar) {			\
	    goto L_overflow_error;		\
	}					\
    } while (0)

    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_again:
	if (is_list(obj)) {
	    while (1) { /* Tail loop */
		while (1) { /* Head loop */
		    if (yield_support && --yield_count <= 0)
			goto L_yield;
		    objp = list_val(obj);
		    /* Head */
		    obj = CAR(objp);
		    if (is_byte(obj)) {
			size++;
			if (size == 0) {
			    goto L_overflow_error;
			}
		    } else if (is_binary(obj) && binary_bitsize(obj) == 0) {
			SAFE_ADD(size, binary_size(obj));
		    } else if (is_list(obj)) {
			ESTACK_PUSH(s, CDR(objp));
			continue; /* Head loop */
		    } else if (is_not_nil(obj)) {
			goto L_type_error;
		    }
		    break;
		}
		/* Tail */
		obj = CDR(objp);
		if (is_list(obj))
		    continue; /* Tail loop */
		else if (is_binary(obj) && binary_bitsize(obj) == 0) {
		    SAFE_ADD(size, binary_size(obj));
		} else if (is_not_nil(obj)) {
		    goto L_type_error;
		}
		break;
	    }
	} else {
	    if (yield_support && --yield_count <= 0)
		goto L_yield;
	    if (is_binary(obj) && binary_bitsize(obj) == 0) { /* Tail was binary */
		SAFE_ADD(size, binary_size(obj));
	    } else if (is_not_nil(obj)) {
		goto L_type_error;
	    }
	}
    }
#undef SAFE_ADD

    *sizep = (ErlDrvSizeT) size;

    res = ERTS_IOLIST_OK;

 L_return:

    DESTROY_ESTACK(s);

    if (yield_support) {
	int yc, reds;
	CLEAR_SAVED_ESTACK(&state->estack);
	yc = init_yield_count - yield_count;
	reds = ((yc - 1) / ERTS_IOLIST_SIZE_YIELDS_COUNT_PER_RED) + 1;
	BUMP_REDS(state->c_p, reds);
	state->reds_left -= reds;
	state->size = (ErlDrvSizeT) size;
	state->have_size = 1;
    }

    return res;

 L_overflow_error:
    res = ERTS_IOLIST_OVERFLOW;
    size = 0;
    goto L_return;

 L_type_error:
    res = ERTS_IOLIST_TYPE;
    size = 0;
    goto L_return;

 L_yield:
    BUMP_ALL_REDS(state->c_p);
    state->reds_left = 0;
    state->size = size;
    state->obj = obj;
    ESTACK_SAVE(s, &state->estack);
    return ERTS_IOLIST_YIELD;
}

int erts_iolist_size_yielding(ErtsIOListState *state)
{
    ErlDrvSizeT size = state->size;
    return iolist_size(1, state, state->obj, &size);
}

int erts_iolist_size(Eterm obj, ErlDrvSizeT* sizep)
{
    *sizep = 0;
    return iolist_size(0, NULL, obj, sizep);
}

/* return 0 if item is not a non-empty flat list of bytes
   otherwise return the nonzero length of the list */
Sint
is_string(Eterm list)
{
    Sint len = 0;

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

static int trim_threshold;
static int top_pad;
static int mmap_threshold;
static int mmap_max;

Uint tot_bin_allocated;

void erts_init_utils(void)
{

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
  int m_opt;
  int *curr_val;

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
    *curr_val = value;
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

char *
erts_read_env(char *key)
{
    size_t value_len = 256;
    char *value = erts_alloc(ERTS_ALC_T_TMP, value_len);
    int res;
    while (1) {
	res = erts_sys_getenv_raw(key, value, &value_len);
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


typedef struct {
    size_t sz;
    char *ptr;
} ErtsEmuArg;

typedef struct {
    int argc;
    ErtsEmuArg *arg;
    size_t no_bytes;
} ErtsEmuArgs;

ErtsEmuArgs saved_emu_args = {0};

void
erts_save_emu_args(int argc, char **argv)
{
#ifdef DEBUG
    char *end_ptr;
#endif
    char *ptr;
    int i;
    size_t arg_sz[100];
    size_t size;

    ASSERT(!saved_emu_args.argc);

    size = sizeof(ErtsEmuArg)*argc;
    for (i = 0; i < argc; i++) {
	size_t sz = sys_strlen(argv[i]);
	if (i < sizeof(arg_sz)/sizeof(arg_sz[0]))
	    arg_sz[i] = sz;
	size += sz+1;
    } 
    ptr = (char *) malloc(size);
    if (!ptr) {
        ERTS_INTERNAL_ERROR("malloc failed to allocate memory!");
    }
#ifdef DEBUG
    end_ptr = ptr + size;
#endif
    saved_emu_args.arg = (ErtsEmuArg *) ptr;
    ptr += sizeof(ErtsEmuArg)*argc;
    saved_emu_args.argc = argc;
    saved_emu_args.no_bytes = 0;
    for (i = 0; i < argc; i++) {
	size_t sz;
	if (i < sizeof(arg_sz)/sizeof(arg_sz[0]))
	    sz = arg_sz[i];
	else
	    sz = sys_strlen(argv[i]);
	saved_emu_args.arg[i].ptr = ptr;
	saved_emu_args.arg[i].sz = sz;
	saved_emu_args.no_bytes += sz;
	ptr += sz+1;
	sys_strcpy(saved_emu_args.arg[i].ptr, argv[i]);
    }
    ASSERT(ptr == end_ptr);
}

Eterm
erts_get_emu_args(Process *c_p)
{
#ifdef DEBUG
    Eterm *end_hp;
#endif
    int i;
    Uint hsz;
    Eterm *hp, res;

    hsz = saved_emu_args.no_bytes*2;
    hsz += saved_emu_args.argc*2;

    hp = HAlloc(c_p, hsz);
#ifdef DEBUG
    end_hp = hp + hsz;
#endif
    res = NIL;

    for (i = saved_emu_args.argc-1; i >= 0; i--) {
    Eterm arg = buf_to_intlist(&hp,
				   saved_emu_args.arg[i].ptr,
				   saved_emu_args.arg[i].sz,
				   NIL);
	res = CONS(hp, arg, res);
	hp += 2;
    }

    ASSERT(hp == end_hp);

    return res;
}


Eterm
erts_get_ethread_info(Process *c_p)
{
    Uint sz, *szp;
    Eterm res, *hp, **hpp, *end_hp = NULL;

    sz = 0;
    szp = &sz;
    hpp = NULL;

    while (1) {
	Eterm tup, list, name;
#if defined(ETHR_NATIVE_ATOMIC32_IMPL)	  \
    || defined(ETHR_NATIVE_ATOMIC64_IMPL)	\
    || defined(ETHR_NATIVE_DW_ATOMIC_IMPL)
	char buf[1024];
	int i;
	char **str;
#endif

	res = NIL;

#ifdef ETHR_X86_MEMBAR_H__

	tup = erts_bld_tuple(hpp, szp, 2,
			     erts_bld_string(hpp, szp, "sse2"),
#ifdef ETHR_X86_RUNTIME_CONF_HAVE_SSE2__
			     erts_bld_string(hpp, szp,
					     (ETHR_X86_RUNTIME_CONF_HAVE_SSE2__
					      ? "yes" : "no"))
#else
			     erts_bld_string(hpp, szp, "yes")
#endif
	    );
	res = erts_bld_cons(hpp, szp, tup, res);

	tup = erts_bld_tuple(hpp, szp, 2,
			     erts_bld_string(hpp, szp,
					     "x86"
#ifdef ARCH_64
					     "_64"
#endif
					     " OOO"),
			     erts_bld_string(hpp, szp,
#ifdef ETHR_X86_OUT_OF_ORDER
					     "yes"
#else
					     "no"
#endif
				 ));

	res = erts_bld_cons(hpp, szp, tup, res);
#endif

#ifdef ETHR_SPARC_V9_MEMBAR_H__

	tup = erts_bld_tuple(hpp, szp, 2,
			     erts_bld_string(hpp, szp, "Sparc V9"),
			     erts_bld_string(hpp, szp,
#if defined(ETHR_SPARC_TSO)
					     "TSO"
#elif defined(ETHR_SPARC_PSO)
					     "PSO"
#elif defined(ETHR_SPARC_RMO)
					     "RMO"
#else
					     "undefined"
#endif
				 ));

	res = erts_bld_cons(hpp, szp, tup, res);

#endif

#ifdef ETHR_PPC_MEMBAR_H__

	tup = erts_bld_tuple(hpp, szp, 2,
			     erts_bld_string(hpp, szp, "lwsync"),
			     erts_bld_string(hpp, szp,
#if defined(ETHR_PPC_HAVE_LWSYNC)
					     "yes"
#elif defined(ETHR_PPC_HAVE_NO_LWSYNC)
					     "no"
#elif defined(ETHR_PPC_RUNTIME_CONF_HAVE_LWSYNC__)
					     ETHR_PPC_RUNTIME_CONF_HAVE_LWSYNC__ ? "yes" : "no"
#else
					     "undefined"
#endif
				 ));

	res = erts_bld_cons(hpp, szp, tup, res);

#endif

	tup = erts_bld_tuple(hpp, szp, 2,
			     erts_bld_string(hpp, szp, "Native rw-spinlocks"),
#ifdef ETHR_NATIVE_RWSPINLOCK_IMPL
			     erts_bld_string(hpp, szp, ETHR_NATIVE_RWSPINLOCK_IMPL)
#else
			     erts_bld_string(hpp, szp, "no")
#endif
	    );
	res = erts_bld_cons(hpp, szp, tup, res);

	tup = erts_bld_tuple(hpp, szp, 2,
			     erts_bld_string(hpp, szp, "Native spinlocks"),
#ifdef ETHR_NATIVE_SPINLOCK_IMPL
			     erts_bld_string(hpp, szp, ETHR_NATIVE_SPINLOCK_IMPL)
#else
			     erts_bld_string(hpp, szp, "no")
#endif
	    );
	res = erts_bld_cons(hpp, szp, tup, res);


	list = NIL;
#ifdef ETHR_NATIVE_DW_ATOMIC_IMPL
	if (ethr_have_native_dw_atomic()) {
	    name = erts_bld_string(hpp, szp, ETHR_NATIVE_DW_ATOMIC_IMPL);
	    str = ethr_native_dw_atomic_ops();
	    for (i = 0; str[i]; i++) {
		erts_snprintf(buf, sizeof(buf), "ethr_native_dw_atomic_%s()", str[i]);
		list = erts_bld_cons(hpp, szp,
				     erts_bld_string(hpp, szp, buf),
				     list);
	    }
	    str = ethr_native_su_dw_atomic_ops();
	    for (i = 0; str[i]; i++) {
		erts_snprintf(buf, sizeof(buf), "ethr_native_su_dw_atomic_%s()", str[i]);
		list = erts_bld_cons(hpp, szp,
				     erts_bld_string(hpp, szp, buf),
				     list);
	    }
	}
	else 
#endif
	    name = erts_bld_string(hpp, szp, "no");

	tup = erts_bld_tuple(hpp, szp, 3,
			     erts_bld_string(hpp, szp, "Double word native atomics"),
			     name,
			     list);
	res = erts_bld_cons(hpp, szp, tup, res);

	list = NIL;
#ifdef ETHR_NATIVE_ATOMIC64_IMPL
	name = erts_bld_string(hpp, szp, ETHR_NATIVE_ATOMIC64_IMPL);
	str = ethr_native_atomic64_ops();
	for (i = 0; str[i]; i++) {
	    erts_snprintf(buf, sizeof(buf), "ethr_native_atomic64_%s()", str[i]);
	    list = erts_bld_cons(hpp, szp,
				 erts_bld_string(hpp, szp, buf),
				 list);
	}
#else
	name = erts_bld_string(hpp, szp, "no");
#endif
	tup = erts_bld_tuple(hpp, szp, 3,
			     erts_bld_string(hpp, szp, "64-bit native atomics"),
			     name,
			     list);
	res = erts_bld_cons(hpp, szp, tup, res);

	list = NIL;
#ifdef ETHR_NATIVE_ATOMIC32_IMPL
	name = erts_bld_string(hpp, szp, ETHR_NATIVE_ATOMIC32_IMPL);
	str = ethr_native_atomic32_ops();
	for (i = 0; str[i]; i++) {
	    erts_snprintf(buf, sizeof(buf), "ethr_native_atomic32_%s()", str[i]);
	    list = erts_bld_cons(hpp, szp,
				erts_bld_string(hpp, szp, buf),
				list);
	}
#else
	name = erts_bld_string(hpp, szp, "no");
#endif
	tup = erts_bld_tuple(hpp, szp, 3,
			     erts_bld_string(hpp, szp, "32-bit native atomics"),
			     name,
			     list);
	res = erts_bld_cons(hpp, szp, tup, res);

	if (hpp) {
	    HRelease(c_p, end_hp, *hpp)
	    return res;
	}

	hp = HAlloc(c_p, sz);
	end_hp = hp + sz;
	hpp = &hp;
	szp = NULL;
    }
}

/*
 * To be used to silence unused result warnings, but do not abuse it.
 */
void erts_silence_warn_unused_result(long unused)
{

}

/*
 * Interval counts
 */
void
erts_interval_init(erts_interval_t *icp)
{
    erts_atomic64_init_nob(&icp->counter.atomic, 0);
#ifdef DEBUG
    icp->smp_api = 0;
#endif
}

void
erts_smp_interval_init(erts_interval_t *icp)
{
#ifdef ERTS_SMP
    erts_interval_init(icp);
#else
    icp->counter.not_atomic = 0;
#endif
#ifdef DEBUG
    icp->smp_api = 1;
#endif
}

static ERTS_INLINE Uint64
step_interval_nob(erts_interval_t *icp)
{
    return (Uint64) erts_atomic64_inc_read_nob(&icp->counter.atomic);
}

static ERTS_INLINE Uint64
step_interval_relb(erts_interval_t *icp)
{
    return (Uint64) erts_atomic64_inc_read_relb(&icp->counter.atomic);
}


static ERTS_INLINE Uint64
ensure_later_interval_nob(erts_interval_t *icp, Uint64 ic)
{
    Uint64 curr_ic;
    curr_ic = (Uint64) erts_atomic64_read_nob(&icp->counter.atomic);
    if (curr_ic > ic)
	return curr_ic;
    return (Uint64) erts_atomic64_inc_read_nob(&icp->counter.atomic);
}


static ERTS_INLINE Uint64
ensure_later_interval_acqb(erts_interval_t *icp, Uint64 ic)
{
    Uint64 curr_ic;
    curr_ic = (Uint64) erts_atomic64_read_acqb(&icp->counter.atomic);
    if (curr_ic > ic)
	return curr_ic;
    return (Uint64) erts_atomic64_inc_read_acqb(&icp->counter.atomic);
}

Uint64
erts_step_interval_nob(erts_interval_t *icp)
{
    ASSERT(!icp->smp_api);
    return step_interval_nob(icp);
}

Uint64
erts_step_interval_relb(erts_interval_t *icp)
{
    ASSERT(!icp->smp_api);
    return step_interval_relb(icp);
}

Uint64
erts_smp_step_interval_nob(erts_interval_t *icp)
{
    ASSERT(icp->smp_api);
#ifdef ERTS_SMP
    return step_interval_nob(icp);
#else
    return ++icp->counter.not_atomic;
#endif
}

Uint64
erts_smp_step_interval_relb(erts_interval_t *icp)
{
    ASSERT(icp->smp_api);
#ifdef ERTS_SMP
    return step_interval_relb(icp);
#else
    return ++icp->counter.not_atomic;
#endif
}

Uint64
erts_ensure_later_interval_nob(erts_interval_t *icp, Uint64 ic)
{
    ASSERT(!icp->smp_api);
    return ensure_later_interval_nob(icp, ic);
}

Uint64
erts_ensure_later_interval_acqb(erts_interval_t *icp, Uint64 ic)
{
    ASSERT(!icp->smp_api);
    return ensure_later_interval_acqb(icp, ic);
}

Uint64
erts_smp_ensure_later_interval_nob(erts_interval_t *icp, Uint64 ic)
{
    ASSERT(icp->smp_api);
#ifdef ERTS_SMP
    return ensure_later_interval_nob(icp, ic);
#else
    if (icp->counter.not_atomic > ic)
	return icp->counter.not_atomic;
    else
	return ++icp->counter.not_atomic;
#endif
}

Uint64
erts_smp_ensure_later_interval_acqb(erts_interval_t *icp, Uint64 ic)
{
    ASSERT(icp->smp_api);
#ifdef ERTS_SMP
    return ensure_later_interval_acqb(icp, ic);
#else
    if (icp->counter.not_atomic > ic)
	return icp->counter.not_atomic;
    else
	return ++icp->counter.not_atomic;
#endif
}

/*
 * A millisecond timestamp without time correction where there's no hrtime
 * - for tracing on "long" things...
 */
Uint64 erts_timestamp_millis(void)
{
#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
    return ERTS_MONOTONIC_TO_MSEC(erts_os_monotonic_time());
#else
    Uint64 res;
    SysTimeval tv;
    sys_gettimeofday(&tv);
    res = (Uint64) tv.tv_sec*1000000;
    res += (Uint64) tv.tv_usec;
    return (res / 1000);
#endif
}

#ifdef DEBUG
/*
 * Handy functions when using a debugger - don't use in the code!
 */

void upp(byte *buf, size_t sz)
{
    bin_write(ERTS_PRINT_STDERR, NULL, buf, sz);
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
    pp(erts_proc_lookup(pid));
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
