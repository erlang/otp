/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2022-2023. All Rights Reserved.
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

#include "sys.h"
#include "global.h"
#include "erl_term_hashing.h"

#include "big.h"
#include "bif.h"
#include "erl_map.h"
#include "erl_binary.h"
#include "erl_bits.h"

#ifdef ERL_INTERNAL_HASH_CRC32C
#   if defined(__x86_64__)
#       include <immintrin.h>
#   elif defined(__aarch64__)
#       include <arm_acle.h>
#   endif
#endif

/*                                                                           *\
 *                                                                           *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* make a hash index from an erlang term */

/*
** There are two hash functions.
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
** Pids are hashed like small numbers but with different constants, as are
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
#define UINT32_HASH_STEP(Expr, Prime1)                                        \
        do {                                                                  \
            Uint32 x = (Uint32) (Expr);                                       \
            hash =                                                            \
                (((((hash)*(Prime1) + (x & 0xFF)) * (Prime1) +                \
                ((x >> 8) & 0xFF)) * (Prime1) +                               \
                ((x >> 16) & 0xFF)) * (Prime1) +                              \
                 (x >> 24));                                                  \
        } while(0)

#define UINT32_HASH_RET(Expr, Prime1, Prime2)                                 \
        UINT32_HASH_STEP(Expr, Prime1);                                       \
        hash = hash * (Prime2);                                               \
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
    case FUN_DEF:
        {
            ErlFunThing* funp = (ErlFunThing *) fun_val(term);

            if (is_local_fun(funp)) {

                ErlFunEntry* fe = funp->entry.fun;
                Uint num_free = funp->num_free;

                hash = hash * FUNNY_NUMBER10 + num_free;
                hash = hash*FUNNY_NUMBER1 +
                        (atom_tab(atom_val(fe->module))->slot.bucket.hvalue);
                hash = hash*FUNNY_NUMBER2 + fe->index;
                hash = hash*FUNNY_NUMBER2 + fe->old_uniq;

                if (num_free > 0) {
                    if (num_free > 1) {
                        WSTACK_PUSH3(stack, (UWord) &funp->env[1],
                                     (num_free-1), MAKE_HASH_TERM_ARRAY_OP);
                    }

                    term = funp->env[0];
                    goto tail_recur;
                }
            } else {
                const ErtsCodeMFA *mfa = &funp->entry.exp->info.mfa;

                ASSERT(is_external_fun(funp) && funp->next == NULL);

                hash = hash * FUNNY_NUMBER11 + mfa->arity;
                hash = hash*FUNNY_NUMBER1 +
                       (atom_tab(atom_val(mfa->module))->slot.bucket.hvalue);
                hash = hash*FUNNY_NUMBER1 +
                        (atom_tab(atom_val(mfa->function))->slot.bucket.hvalue);
            }
            break;
        }
    case PID_DEF:
        /* only 15 bits... */
        UINT32_HASH_RET(internal_pid_number(term),FUNNY_NUMBER5,FUNNY_NUMBER6);
    case EXTERNAL_PID_DEF:
        /* only 15 bits... */
        UINT32_HASH_RET(external_pid_number(term),FUNNY_NUMBER5,FUNNY_NUMBER6);
    case PORT_DEF:
    case EXTERNAL_PORT_DEF: {
        Uint64 number = port_number(term);
        Uint32 low = (Uint32) (number & 0xffffffff);
        Uint32 high = (Uint32) ((number >> 32) & 0xffffffff);
        if (high)
            UINT32_HASH_STEP(high, FUNNY_NUMBER11);
        UINT32_HASH_RET(low,FUNNY_NUMBER9,FUNNY_NUMBER10);
    }
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

#undef MAKE_HASH_TUPLE_OP
#undef MAKE_HASH_TERM_ARRAY_OP
#undef MAKE_HASH_CDR_PRE_OP
#undef MAKE_HASH_CDR_POST_OP
#undef UINT32_HASH_STEP
#undef UINT32_HASH_RET
}

/* Hash function suggested by Bob Jenkins. */
#define MIX(a,b,c)                                                            \
    do {                                                                      \
        a -= b; a -= c; a ^= (c>>13);                                         \
        b -= c; b -= a; b ^= (a<<8);                                          \
        c -= a; c -= b; c ^= (b>>13);                                         \
        a -= b; a -= c; a ^= (c>>12);                                         \
        b -= c; b -= a; b ^= (a<<16);                                         \
        c -= a; c -= b; c ^= (b>>5);                                          \
        a -= b; a -= c; a ^= (c>>3);                                          \
        b -= c; b -= a; b ^= (a<<10);                                         \
        c -= a; c -= b; c ^= (b>>15);                                         \
    } while(0)

#define HCONST 0x9e3779b9UL /* the golden ratio; an arbitrary value */

#define BLOCK_HASH_BYTES_PER_ITER 12

/* The three functions below are separated into different functions even
   though they are always used together to make trapping and handling
   of unaligned binaries easier. Examples of how they are used can be
   found in block_hash and make_hash2_helper.*/
static ERTS_INLINE
void block_hash_setup(Uint32 initval,
                      ErtsBlockHashHelperCtx* ctx /* out parameter */)
{
    ctx->a = ctx->b = HCONST;
    ctx->c = initval;           /* the previous hash value */
}

static ERTS_INLINE
void block_hash_buffer(byte *buf,
                       Uint buf_length,
                       ErtsBlockHashHelperCtx* ctx /* out parameter */)
{
    Uint len = buf_length;
    byte *k = buf;
    ASSERT(buf_length % BLOCK_HASH_BYTES_PER_ITER == 0);
    while (len >= BLOCK_HASH_BYTES_PER_ITER) {
        ctx->a += (k[0] +((Uint32)k[1]<<8) +((Uint32)k[2]<<16) +((Uint32)k[3]<<24));
        ctx->b += (k[4] +((Uint32)k[5]<<8) +((Uint32)k[6]<<16) +((Uint32)k[7]<<24));
        ctx->c += (k[8] +((Uint32)k[9]<<8) +((Uint32)k[10]<<16)+((Uint32)k[11]<<24));
        MIX(ctx->a,ctx->b,ctx->c);
        k += BLOCK_HASH_BYTES_PER_ITER; len -= BLOCK_HASH_BYTES_PER_ITER;
    }
}

static ERTS_INLINE
Uint32 block_hash_final_bytes(byte *buf,
                              Uint buf_length,
                              Uint full_length,
                              ErtsBlockHashHelperCtx* ctx)
{
    Uint len = buf_length;
    byte *k = buf;
    ctx->c += full_length;
    switch(len)
    { /* all the case statements fall through */      
    case 11: ctx->c+=((Uint32)k[10]<<24);
    case 10: ctx->c+=((Uint32)k[9]<<16);
    case 9 : ctx->c+=((Uint32)k[8]<<8);
    /* the first byte of c is reserved for the length */
    case 8 : ctx->b+=((Uint32)k[7]<<24);
    case 7 : ctx->b+=((Uint32)k[6]<<16);
    case 6 : ctx->b+=((Uint32)k[5]<<8);
    case 5 : ctx->b+=k[4];
    case 4 : ctx->a+=((Uint32)k[3]<<24);
    case 3 : ctx->a+=((Uint32)k[2]<<16);
    case 2 : ctx->a+=((Uint32)k[1]<<8);
    case 1 : ctx->a+=k[0];
    /* case 0: nothing left to add */
    }
    MIX(ctx->a,ctx->b,ctx->c);
    return ctx->c;
}

static
Uint32
block_hash(byte *block, Uint block_length, Uint32 initval)
{
    ErtsBlockHashHelperCtx ctx;
    Uint no_bytes_not_in_loop =
        (block_length % BLOCK_HASH_BYTES_PER_ITER);
    Uint no_bytes_to_process_in_loop =
        block_length - no_bytes_not_in_loop;
    byte *final_bytes = block + no_bytes_to_process_in_loop;
    block_hash_setup(initval, &ctx);
    block_hash_buffer(block,
                      no_bytes_to_process_in_loop,
                      &ctx);
    return block_hash_final_bytes(final_bytes,
                                  no_bytes_not_in_loop,
                                  block_length,
                                  &ctx);
}

/*
 * Note! erts_block_hash() and erts_iov_block_hash() *must* produce
 * the same result if the I/O vector is flattened and contain the
 * same bytes as the array.
 */

void
erts_block_hash_init(ErtsBlockHashState *state, const byte *ptr,
                     Uint len, Uint32 initval)
{
    block_hash_setup(initval, &state->hctx);
    state->ptr = ptr;
    state->len = len;
    state->tot_len = len;
}

int
erts_block_hash(Uint32 *hashp, Uint *sizep, ErtsBlockHashState *state)
{
    byte *ptr = (byte *) state->ptr;
    Uint len = state->len;
    Sint flen;
    Uint llen;

    do {

        if (*sizep < len) {
            llen = *sizep;
            llen -= llen % BLOCK_HASH_BYTES_PER_ITER;
            if (len > llen + BLOCK_HASH_BYTES_PER_ITER) {
                llen += BLOCK_HASH_BYTES_PER_ITER;
                flen = -1;
                break;
            }
        }

        /* do it all... */
        flen = len % BLOCK_HASH_BYTES_PER_ITER;
        llen = len - flen;

    } while (0);

    block_hash_buffer(ptr, llen, &state->hctx);

    ptr += llen;

    if (flen < 0) {
        state->ptr = ptr;
        state->len -= llen;
        *sizep = llen;
        return 0; /* yield */
    }

    *hashp = block_hash_final_bytes(ptr, (Uint) flen,
                                    state->tot_len, &state->hctx);

    *sizep = llen + flen;

    state->ptr = ptr + flen;
    state->len = 0;

    return !0; /* done */
}

/*
 * Note! erts_block_hash() and erts_iov_block_hash() *must* produce
 * the same result if the I/O vector is flattened and contain the
 * same bytes as the array.
 */

void
erts_iov_block_hash_init(ErtsIovBlockHashState *state, SysIOVec *iov,
                         Uint vlen, Uint32 initval)
{
    block_hash_setup(initval, &state->hctx);
    state->iov = iov;
    state->vlen = vlen;
    state->tot_len = 0;
    state->vix = 0;
    state->ix = 0;
}

int
erts_iov_block_hash(Uint32 *hashp, Uint *sizep, ErtsIovBlockHashState *state)
{
    byte buf[BLOCK_HASH_BYTES_PER_ITER];
    ErtsBlockHashHelperCtx *hctx = &state->hctx;
    SysIOVec *iov = state->iov;
    Uint vlen = state->vlen;
    int vix = state->vix;
    int ix = state->ix;
    Uint cix = 0;
    byte *final_bytes;
    Uint no_final_bytes;
    Uint chunk_sz = (*sizep
                     - *sizep % BLOCK_HASH_BYTES_PER_ITER
                     + BLOCK_HASH_BYTES_PER_ITER);

    do {
        Uint bsz, csz;
        int left;
        byte *ptr;

        ASSERT((cix % BLOCK_HASH_BYTES_PER_ITER) == 0);

        /*
         * We may have empty vectors...
         */
        while (ix == iov[vix].iov_len) {
            vix++;
            if (vix == vlen) {
                final_bytes = NULL;
                no_final_bytes = 0;
                goto finalize;
            }
            ix = 0;
        }

        csz = chunk_sz - cix;
        left = iov[vix].iov_len - ix;
        ptr = iov[vix].iov_base;

        if (left >= BLOCK_HASH_BYTES_PER_ITER) {
            if (csz <= left)
                bsz = csz;
            else
                bsz = left - (left % BLOCK_HASH_BYTES_PER_ITER);
            block_hash_buffer(ptr + ix, bsz, hctx);
            cix += bsz;
            ix += bsz;
        }
        else {
            int bix = 0;
            bsz = left;
            while (!0) {
                sys_memcpy(&buf[bix], ptr + ix, bsz);
                bix += bsz;
                cix += bsz;
                ix += bsz;
                if (bix == BLOCK_HASH_BYTES_PER_ITER) {
                    block_hash_buffer(&buf[0],
                                      (Uint) BLOCK_HASH_BYTES_PER_ITER,
                                      hctx);
                    break;
                }
                ASSERT(ix == iov[vix].iov_len);
                vix++;
                if (vix == vlen) {
                    final_bytes = &buf[0];
                    no_final_bytes = (Uint) bsz;
                    goto finalize;
                }
                ix = 0;
                ptr = iov[vix].iov_base;
                bsz = iov[vix].iov_len;
                if (bsz > BLOCK_HASH_BYTES_PER_ITER - bix)
                    bsz = BLOCK_HASH_BYTES_PER_ITER - bix;
            }
        }

    } while (cix < chunk_sz);

    ASSERT((cix % BLOCK_HASH_BYTES_PER_ITER) == 0);

    /* yield */

    *sizep = cix;

    state->tot_len += cix;
    state->vix = vix;
    state->ix = ix;

    return 0;

finalize:

    state->tot_len += cix;
    *sizep = cix;

    *hashp = block_hash_final_bytes(final_bytes, no_final_bytes,
                                    state->tot_len, hctx);
    return !0; /* done */
}



typedef enum {
    tag_primary_list,
    arityval_subtag,
    hamt_subtag_head_flatmap,
    map_subtag,
    fun_subtag,
    neg_big_subtag,
    sub_binary_subtag_1,
    sub_binary_subtag_2,
    hash2_common_1,
    hash2_common_2,
    hash2_common_3,
} ErtsMakeHash2TrapLocation; 

typedef struct {
    int c;
    Uint32 sh;
    Eterm* ptr;
} ErtsMakeHash2Context_TAG_PRIMARY_LIST;

typedef struct {
    int i;
    int arity;
    Eterm* elem;
} ErtsMakeHash2Context_ARITYVAL_SUBTAG;

typedef struct {
    Eterm *ks;
    Eterm *vs;
    int i;
    Uint size;
} ErtsMakeHash2Context_HAMT_SUBTAG_HEAD_FLATMAP;

typedef struct {
    Eterm* ptr;
    int i;
} ErtsMakeHash2Context_MAP_SUBTAG;

typedef struct {
    Uint num_free;
    Eterm* bptr;
} ErtsMakeHash2Context_FUN_SUBTAG;

typedef struct {
    Eterm* ptr;
    Uint i;
    Uint n;
    Uint32 con;
} ErtsMakeHash2Context_NEG_BIG_SUBTAG;

typedef struct {
    byte* bptr;
    Uint sz;
    Uint bitsize;
    Uint bitoffs;
    Uint no_bytes_processed;
    ErtsBlockHashHelperCtx block_hash_ctx;
    /* The following fields are only used when bitoffs != 0 */
    byte* buf;
    int done;

} ErtsMakeHash2Context_SUB_BINARY_SUBTAG;

typedef struct {
    int dummy__; /* Empty structs are not supported on all platforms */
} ErtsMakeHash2Context_EMPTY;

typedef struct {
    ErtsMakeHash2TrapLocation trap_location;
    /* specific to the trap location: */
    union {
        ErtsMakeHash2Context_TAG_PRIMARY_LIST tag_primary_list;
        ErtsMakeHash2Context_ARITYVAL_SUBTAG arityval_subtag;
        ErtsMakeHash2Context_HAMT_SUBTAG_HEAD_FLATMAP hamt_subtag_head_flatmap;
        ErtsMakeHash2Context_MAP_SUBTAG map_subtag;
        ErtsMakeHash2Context_FUN_SUBTAG fun_subtag;
        ErtsMakeHash2Context_NEG_BIG_SUBTAG neg_big_subtag;
        ErtsMakeHash2Context_SUB_BINARY_SUBTAG sub_binary_subtag_1;
        ErtsMakeHash2Context_SUB_BINARY_SUBTAG sub_binary_subtag_2;
        ErtsMakeHash2Context_EMPTY hash2_common_1;
        ErtsMakeHash2Context_EMPTY hash2_common_2;
        ErtsMakeHash2Context_EMPTY hash2_common_3;
    } trap_location_state;
    /* same for all trap locations: */
    Eterm term; 
    Uint32 hash;
    Uint32 hash_xor_pairs;
    ErtsEStack stack;
} ErtsMakeHash2Context;

static int make_hash2_ctx_bin_dtor(Binary *context_bin) {
    ErtsMakeHash2Context* context = ERTS_MAGIC_BIN_DATA(context_bin);
    DESTROY_SAVED_ESTACK(&context->stack);
    if (context->trap_location == sub_binary_subtag_2 &&
        context->trap_location_state.sub_binary_subtag_2.buf != NULL) {
        erts_free(ERTS_ALC_T_PHASH2_TRAP, context->trap_location_state.sub_binary_subtag_2.buf);
    }
    return 1;
}

/* hash2_save_trap_state is called seldom so we want to avoid inlining */
static ERTS_NOINLINE
Eterm hash2_save_trap_state(Eterm state_mref,
                            Uint32 hash_xor_pairs,
                            Uint32 hash,
                            Process* p,
                            Eterm term,
                            Eterm* ESTK_DEF_STACK(s),
                            ErtsEStack s,
                            ErtsMakeHash2TrapLocation trap_location,
                            void* trap_location_state_ptr,
                            size_t trap_location_state_size) {
    Binary* state_bin;
    ErtsMakeHash2Context* context;
    if (state_mref == THE_NON_VALUE) {
        Eterm* hp;
        state_bin = erts_create_magic_binary(sizeof(ErtsMakeHash2Context),
                                             make_hash2_ctx_bin_dtor);
        hp = HAlloc(p, ERTS_MAGIC_REF_THING_SIZE);
        state_mref = erts_mk_magic_ref(&hp, &MSO(p), state_bin);
    } else {
        state_bin = erts_magic_ref2bin(state_mref);
    }
    context = ERTS_MAGIC_BIN_DATA(state_bin);
    context->term = term;
    context->hash = hash;
    context->hash_xor_pairs = hash_xor_pairs;
    ESTACK_SAVE(s, &context->stack);
    context->trap_location = trap_location;
    sys_memcpy(&context->trap_location_state,
               trap_location_state_ptr,
               trap_location_state_size);
    erts_set_gc_state(p, 0);
    BUMP_ALL_REDS(p);
    return state_mref;
}
#undef NOINLINE_HASH2_SAVE_TRAP_STATE

/* Writes back a magic reference to *state_mref_write_back when the
   function traps */
static ERTS_INLINE Uint32
make_hash2_helper(Eterm term_param, const int can_trap, Eterm* state_mref_write_back, Process* p)
{
    static const Uint ITERATIONS_PER_RED = 64;
    Uint32 hash;
    Uint32 hash_xor_pairs;
    Eterm term = term_param;
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

#define UINT32_HASH_2(Expr1, Expr2, AConst)                                   \
         do {                                                                 \
            Uint32 a,b;                                                       \
            a = AConst + (Uint32) (Expr1);                                    \
            b = AConst + (Uint32) (Expr2);                                    \
            MIX(a,b,hash);                                                    \
         } while(0)

#define UINT32_HASH(Expr, AConst) UINT32_HASH_2(Expr, 0, AConst)

#define SINT32_HASH(Expr, AConst)                                             \
        do {                                                                  \
            Sint32 y = (Sint32) (Expr);                                       \
            if (y < 0) {                                                      \
                UINT32_HASH(-y, AConst);                                      \
                /* Negative numbers are unnecessarily mixed twice. */         \
            }                                                                 \
            UINT32_HASH(y, AConst);                                           \
        } while(0)

#define IS_SSMALL28(x) (((Uint) (((x) >> (28-1)) + 1)) < 2)

#define NOT_SSMALL28_HASH(SMALL)                                              \
    do {                                                                      \
        Uint64 t;                                                             \
        Uint32 x, y;                                                          \
        Uint32 con;                                                           \
        if (SMALL < 0) {                                                      \
            con = HCONST_10;                                                  \
            t = (Uint64)(SMALL * (-1));                                       \
        } else {                                                              \
            con = HCONST_11;                                                  \
            t = SMALL;                                                        \
        }                                                                     \
        x = t & 0xffffffff;                                                   \
        y = t >> 32;                                                          \
        UINT32_HASH_2(x, y, con);                                             \
    } while(0)
    
#ifdef ARCH_64
#  define POINTER_HASH(Ptr, AConst) UINT32_HASH_2((Uint32)(UWord)(Ptr), (((UWord)(Ptr)) >> 32), AConst)
#else
#  define POINTER_HASH(Ptr, AConst) UINT32_HASH(Ptr, AConst)
#endif

#define TRAP_LOCATION_NO_RED(location_name)                                   \
    do {                                                                      \
        if(can_trap && iterations_until_trap <= 0) {                          \
                *state_mref_write_back  =                                     \
                    hash2_save_trap_state(state_mref,                         \
                                          hash_xor_pairs,                     \
                                          hash,                               \
                                          p,                                  \
                                          term,                               \
                                          ESTK_DEF_STACK(s),                  \
                                          s,                                  \
                                          location_name,                      \
                                          &ctx,                               \
                                          sizeof(ctx));                       \
                return 0;                                                     \
            L_##location_name:                                                \
                ctx = context->trap_location_state. location_name;            \
        }                                                                     \
    } while(0)

#define TRAP_LOCATION(location_name)                                          \
    do {                                                                      \
        if (can_trap) {                                                       \
            iterations_until_trap--;                                          \
            TRAP_LOCATION_NO_RED(location_name);                              \
        }                                                                     \
    } while(0)

#define TRAP_LOCATION_NO_CTX(location_name)                                   \
    do {                                                                      \
        ErtsMakeHash2Context_EMPTY ctx;                                       \
        TRAP_LOCATION(location_name);                                         \
    } while(0)
    
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
              Sint small = signed_val(term);
              if (SMALL_BITS > 28 && !IS_SSMALL28(small)) {
                  hash = 0;
                  NOT_SSMALL28_HASH(small);
                  return hash;
              }
              hash = 0;
              SINT32_HASH(small, HCONST);
              return hash;
          }
        }
    };
    {
    Eterm tmp;
    long max_iterations = 0;
    long iterations_until_trap = 0;
    Eterm state_mref = THE_NON_VALUE;
    ErtsMakeHash2Context* context = NULL;
    DECLARE_ESTACK(s);
    ESTACK_CHANGE_ALLOCATOR(s, ERTS_ALC_T_SAVED_ESTACK);
    if(can_trap){
#ifdef DEBUG
        (void)ITERATIONS_PER_RED;
        iterations_until_trap = max_iterations =
            (1103515245 * (ERTS_BIF_REDS_LEFT(p)) + 12345)  % 227;
#else
        iterations_until_trap = max_iterations =
            ITERATIONS_PER_RED * ERTS_BIF_REDS_LEFT(p);
#endif
    }
    if (can_trap && is_internal_magic_ref(term)) {
        Binary* state_bin;
        state_mref = term;
        state_bin = erts_magic_ref2bin(state_mref);
        if (ERTS_MAGIC_BIN_DESTRUCTOR(state_bin) == make_hash2_ctx_bin_dtor) {
            /* Restore state after a trap */
            context = ERTS_MAGIC_BIN_DATA(state_bin);
            term = context->term;
            hash = context->hash;
            hash_xor_pairs = context->hash_xor_pairs;
            ESTACK_RESTORE(s, &context->stack);
            ASSERT(p->flags & F_DISABLE_GC);
            erts_set_gc_state(p, 1);
            switch (context->trap_location) {
            case hash2_common_3:           goto L_hash2_common_3;
            case tag_primary_list:         goto L_tag_primary_list;
            case arityval_subtag:          goto L_arityval_subtag;
            case hamt_subtag_head_flatmap: goto L_hamt_subtag_head_flatmap;
            case map_subtag:               goto L_map_subtag;
            case fun_subtag:               goto L_fun_subtag;
            case neg_big_subtag:           goto L_neg_big_subtag;
            case sub_binary_subtag_1:      goto L_sub_binary_subtag_1;
            case sub_binary_subtag_2:      goto L_sub_binary_subtag_2;
            case hash2_common_1:           goto L_hash2_common_1;
            case hash2_common_2:           goto L_hash2_common_2;
            }
        }
    }
    hash = 0;
    for (;;) {
        switch (primary_tag(term)) {
        case TAG_PRIMARY_LIST:
        {
            ErtsMakeHash2Context_TAG_PRIMARY_LIST ctx = {
                .c =  0,
                .sh = 0,
                .ptr = list_val(term)};
            while (is_byte(*ctx.ptr)) {
                /* Optimization for strings. */
                ctx.sh = (ctx.sh << 8) + unsigned_val(*ctx.ptr);
                if (ctx.c == 3) {
                    UINT32_HASH(ctx.sh, HCONST_4);
                    ctx.c = ctx.sh = 0;
                } else {
                    ctx.c++;
                }
                term = CDR(ctx.ptr);
                if (is_not_list(term))
                    break;
                ctx.ptr = list_val(term);
                TRAP_LOCATION(tag_primary_list);
            }
            if (ctx.c > 0)
                UINT32_HASH(ctx.sh, HCONST_4);
            if (is_list(term)) {
                tmp = CDR(ctx.ptr);
                ESTACK_PUSH(s, tmp);
                term = CAR(ctx.ptr);
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
                ErtsMakeHash2Context_ARITYVAL_SUBTAG ctx = {
                    .i =  0,
                    .arity = header_arity(hdr),
                    .elem = tuple_val(term)};
                UINT32_HASH(ctx.arity, HCONST_9);
                if (ctx.arity == 0) /* Empty tuple */
                    goto hash2_common;
                for (ctx.i = ctx.arity; ; ctx.i--) {
                    term = ctx.elem[ctx.i];
                    if (ctx.i == 1)
                        break;
                    ESTACK_PUSH(s, term);
                    TRAP_LOCATION(arityval_subtag);
                }
            }
            break;
            case MAP_SUBTAG:
            {
                Uint size;
                ErtsMakeHash2Context_MAP_SUBTAG ctx = {
                    .ptr = boxed_val(term) + 1,
                    .i = 0};
                switch (hdr & _HEADER_MAP_SUBTAG_MASK) {
                case HAMT_SUBTAG_HEAD_FLATMAP:
                {
                    flatmap_t *mp = (flatmap_t *)flatmap_val(term);
                    ErtsMakeHash2Context_HAMT_SUBTAG_HEAD_FLATMAP ctx = {
                        .ks = flatmap_get_keys(mp),
                        .vs = flatmap_get_values(mp),
                        .i = 0,
                        .size = flatmap_get_size(mp)};
                    UINT32_HASH(ctx.size, HCONST_16);
                    if (ctx.size == 0)
                        goto hash2_common;

                    /* We want a portable hash function that is *independent* of
                     * the order in which keys and values are encountered.
                     * We therefore calculate context independent hashes for all
                     * key-value pairs and then xor them together.
                     */
                    ESTACK_PUSH(s, hash_xor_pairs);
                    ESTACK_PUSH(s, hash);
                    ESTACK_PUSH(s, HASH_MAP_TAIL);
                    hash = 0;
                    hash_xor_pairs = 0;
                    for (ctx.i = ctx.size - 1; ctx.i >= 0; ctx.i--) {
                        ESTACK_PUSH(s, HASH_MAP_PAIR);
                        ESTACK_PUSH(s, ctx.vs[ctx.i]);
                        ESTACK_PUSH(s, ctx.ks[ctx.i]);
                        TRAP_LOCATION(hamt_subtag_head_flatmap);
                    }
                    goto hash2_common;
                }

                case HAMT_SUBTAG_HEAD_ARRAY:
                case HAMT_SUBTAG_HEAD_BITMAP:
                    size = *ctx.ptr++;
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
                    ctx.i = 16;
                    break;
                case HAMT_SUBTAG_HEAD_BITMAP:
                case HAMT_SUBTAG_NODE_BITMAP:
                    ctx.i = hashmap_bitcount(MAP_HEADER_VAL(hdr));
                    break;
                default:
                    erts_exit(ERTS_ERROR_EXIT, "bad header");
                }
                while (ctx.i) {
                    if (is_list(*ctx.ptr)) {
                        Eterm* cons = list_val(*ctx.ptr);
                        ESTACK_PUSH(s, HASH_MAP_PAIR);
                        ESTACK_PUSH(s, CDR(cons));
                        ESTACK_PUSH(s, CAR(cons));
                    }
                    else {
                        ASSERT(is_boxed(*ctx.ptr));
                        if (is_tuple(*ctx.ptr)) { /* collision node */
                            Eterm *coll_ptr = tuple_val(*ctx.ptr);
                            Uint n = arityval(*coll_ptr);
                            ASSERT(n >= 2);
                            coll_ptr++;
                            for (; n; n--, coll_ptr++) {
                                Eterm* cons = list_val(*coll_ptr);
                                ESTACK_PUSH3(s, HASH_MAP_PAIR, CDR(cons), CAR(cons));
                            }
                        }
                        else
                            ESTACK_PUSH(s, *ctx.ptr);
                    }
                    ctx.i--; ctx.ptr++;
                    TRAP_LOCATION(map_subtag);
                }
                goto hash2_common;
            }
            break;

            case FUN_SUBTAG:
            {
                ErlFunThing* funp = (ErlFunThing *) fun_val(term);

                if (is_local_fun(funp)) {
                    ErlFunEntry* fe = funp->entry.fun;
                    ErtsMakeHash2Context_FUN_SUBTAG ctx = {
                        .num_free = funp->num_free,
                        .bptr = NULL};

                    UINT32_HASH_2
                        (ctx.num_free,
                         atom_tab(atom_val(fe->module))->slot.bucket.hvalue,
                         HCONST);
                    UINT32_HASH_2
                        (fe->index, fe->old_uniq, HCONST);
                    if (ctx.num_free == 0) {
                        goto hash2_common;
                    } else {
                        ctx.bptr = funp->env + ctx.num_free - 1;
                        while (ctx.num_free-- > 1) {
                            term = *ctx.bptr--;
                            ESTACK_PUSH(s, term);
                        TRAP_LOCATION(fun_subtag);
                        }
                        term = *ctx.bptr;
                    }
                } else {
                    Export *ep = funp->entry.exp;

                    ASSERT(is_external_fun(funp) && funp->next == NULL);

                    UINT32_HASH_2
                        (ep->info.mfa.arity,
                         atom_tab(atom_val(ep->info.mfa.module))->slot.bucket.hvalue,
                         HCONST);
                    UINT32_HASH
                        (atom_tab(atom_val(ep->info.mfa.function))->slot.bucket.hvalue,
                         HCONST_14);

                    goto hash2_common;
                }
            }
            break;
            case REFC_BINARY_SUBTAG:
            case HEAP_BINARY_SUBTAG:
            case SUB_BINARY_SUBTAG:
            {
#define BYTE_BITS 8
                ErtsMakeHash2Context_SUB_BINARY_SUBTAG ctx = {
                    .bptr = 0,
                    /* !!!!!!!!!!!!!!!!!!!! OBS !!!!!!!!!!!!!!!!!!!!
                     *
                     * The size is truncated to 32 bits on the line
                     * below so that the code is compatible with old
                     * versions of the code. This means that hash
                     * values for binaries with a size greater than
                     * 4GB do not take all bytes in consideration.
                     *
                     * !!!!!!!!!!!!!!!!!!!! OBS !!!!!!!!!!!!!!!!!!!!
                     */ 
                    .sz = (0xFFFFFFFF & binary_size(term)),
                    .bitsize = 0,
                    .bitoffs = 0,
                    .no_bytes_processed = 0
                };
                Uint32 con = HCONST_13 + hash;
                Uint iters_for_bin = MAX(1, ctx.sz / BLOCK_HASH_BYTES_PER_ITER);
                ERTS_GET_BINARY_BYTES(term, ctx.bptr, ctx.bitoffs, ctx.bitsize);
                if (ctx.sz == 0 && ctx.bitsize == 0) {
                    hash = con;
                } else if (ctx.bitoffs == 0 &&
                           (!can_trap ||
                            (iterations_until_trap - iters_for_bin) > 0)) {
                    /* No need to trap while hashing binary */
                    if (can_trap) iterations_until_trap -= iters_for_bin;
                    hash = block_hash(ctx.bptr, ctx.sz, con);
                    if (ctx.bitsize > 0) {
                        UINT32_HASH_2(ctx.bitsize,
                                      (ctx.bptr[ctx.sz] >> (BYTE_BITS - ctx.bitsize)),
                                      HCONST_15);
                    }
                } else if (ctx.bitoffs == 0) {
                    /* Need to trap while hashing binary */
                    ErtsBlockHashHelperCtx* block_hash_ctx = &ctx.block_hash_ctx;
                    block_hash_setup(con, block_hash_ctx);
                    do {
                        Uint max_bytes_to_process =
                            iterations_until_trap <= 0 ? BLOCK_HASH_BYTES_PER_ITER :
                            iterations_until_trap * BLOCK_HASH_BYTES_PER_ITER;
                        Uint bytes_left = ctx.sz - ctx.no_bytes_processed;
                        Uint even_bytes_left =
                            bytes_left - (bytes_left % BLOCK_HASH_BYTES_PER_ITER);
                        Uint bytes_to_process =
                            MIN(max_bytes_to_process, even_bytes_left);
                        block_hash_buffer(&ctx.bptr[ctx.no_bytes_processed],
                                          bytes_to_process,
                                          block_hash_ctx);
                        ctx.no_bytes_processed += bytes_to_process;
                        iterations_until_trap -=
                            MAX(1, bytes_to_process / BLOCK_HASH_BYTES_PER_ITER);
                        TRAP_LOCATION_NO_RED(sub_binary_subtag_1);
                        block_hash_ctx = &ctx.block_hash_ctx; /* Restore after trap */
                    } while ((ctx.sz - ctx.no_bytes_processed) >=
                             BLOCK_HASH_BYTES_PER_ITER);
                    hash = block_hash_final_bytes(ctx.bptr +
                                                  ctx.no_bytes_processed,
                                                  ctx.sz - ctx.no_bytes_processed,
                                                  ctx.sz,
                                                  block_hash_ctx);
                    if (ctx.bitsize > 0) {
                        UINT32_HASH_2(ctx.bitsize,
                                      (ctx.bptr[ctx.sz] >> (BYTE_BITS - ctx.bitsize)),
                                      HCONST_15);
                    }
                } else if (/* ctx.bitoffs != 0 && */
                           (!can_trap ||
                            (iterations_until_trap - iters_for_bin) > 0)) {
                    /* No need to trap while hashing binary */
                    Uint nr_of_bytes = ctx.sz + (ctx.bitsize != 0);
                    byte *buf = erts_alloc(ERTS_ALC_T_TMP, nr_of_bytes);
                    Uint nr_of_bits_to_copy = ctx.sz*BYTE_BITS+ctx.bitsize;
                    if (can_trap) iterations_until_trap -= iters_for_bin;
                    erts_copy_bits(ctx.bptr,
                                   ctx.bitoffs, 1, buf, 0, 1, nr_of_bits_to_copy);
                    hash = block_hash(buf, ctx.sz, con);
                    if (ctx.bitsize > 0) {
                        UINT32_HASH_2(ctx.bitsize,
                                      (buf[ctx.sz] >> (BYTE_BITS - ctx.bitsize)),
                                      HCONST_15);
                    }
                    erts_free(ERTS_ALC_T_TMP, buf);
                } else /* ctx.bitoffs != 0 && */ {
#ifdef DEBUG
#define BINARY_BUF_SIZE (BLOCK_HASH_BYTES_PER_ITER * 3)
#else
#define BINARY_BUF_SIZE (BLOCK_HASH_BYTES_PER_ITER * 256)
#endif
#define BINARY_BUF_SIZE_BITS (BINARY_BUF_SIZE*BYTE_BITS)
                    /* Need to trap while hashing binary */
                    ErtsBlockHashHelperCtx* block_hash_ctx = &ctx.block_hash_ctx;
                    Uint nr_of_bytes = ctx.sz + (ctx.bitsize != 0);
                    ERTS_CT_ASSERT(BINARY_BUF_SIZE % BLOCK_HASH_BYTES_PER_ITER == 0);
                    ctx.buf = erts_alloc(ERTS_ALC_T_PHASH2_TRAP,
                                         MIN(nr_of_bytes, BINARY_BUF_SIZE));
                    block_hash_setup(con, block_hash_ctx);
                    do {
                        Uint bytes_left =
                            ctx.sz - ctx.no_bytes_processed;
                        Uint even_bytes_left =
                            bytes_left - (bytes_left % BLOCK_HASH_BYTES_PER_ITER);
                        Uint bytes_to_process =
                            MIN(BINARY_BUF_SIZE, even_bytes_left);
                        Uint nr_of_bits_left =
                            (ctx.sz*BYTE_BITS+ctx.bitsize) -
                            ctx.no_bytes_processed*BYTE_BITS; 
                        Uint nr_of_bits_to_copy =
                            MIN(nr_of_bits_left, BINARY_BUF_SIZE_BITS);
                        ctx.done = nr_of_bits_left == nr_of_bits_to_copy;
                        erts_copy_bits(ctx.bptr + ctx.no_bytes_processed,
                                       ctx.bitoffs, 1, ctx.buf, 0, 1,
                                       nr_of_bits_to_copy);
                        block_hash_buffer(ctx.buf,
                                          bytes_to_process,
                                          block_hash_ctx);
                        ctx.no_bytes_processed += bytes_to_process;
                        iterations_until_trap -=
                            MAX(1, bytes_to_process / BLOCK_HASH_BYTES_PER_ITER);
                        TRAP_LOCATION_NO_RED(sub_binary_subtag_2);
                        block_hash_ctx = &ctx.block_hash_ctx; /* Restore after trap */
                    } while (!ctx.done);
                    nr_of_bytes = ctx.sz + (ctx.bitsize != 0);
                    hash = block_hash_final_bytes(ctx.buf +
                                                  (ctx.no_bytes_processed -
                                                   ((nr_of_bytes-1) / BINARY_BUF_SIZE) *  BINARY_BUF_SIZE),
                                                  ctx.sz - ctx.no_bytes_processed,
                                                  ctx.sz,
                                                  block_hash_ctx);
                    if (ctx.bitsize > 0) {
                        Uint last_byte_index =
                            nr_of_bytes - (((nr_of_bytes-1) / BINARY_BUF_SIZE) *  BINARY_BUF_SIZE) -1;
                        UINT32_HASH_2(ctx.bitsize,
                                      (ctx.buf[last_byte_index] >> (BYTE_BITS - ctx.bitsize)),
                                      HCONST_15);
                    }
                    erts_free(ERTS_ALC_T_PHASH2_TRAP, ctx.buf);
                    context->trap_location_state.sub_binary_subtag_2.buf = NULL;
                }
                goto hash2_common;
#undef BYTE_BITS
#undef BINARY_BUF_SIZE
#undef BINARY_BUF_SIZE_BITS
            }
            break;
            case POS_BIG_SUBTAG:
            case NEG_BIG_SUBTAG:
            {
                Eterm* big_val_ptr = big_val(term);
                ErtsMakeHash2Context_NEG_BIG_SUBTAG ctx = {
                    .ptr = big_val_ptr,
                    .i = 0,
                    .n = BIG_SIZE(big_val_ptr),
                    .con = BIG_SIGN(big_val_ptr) ? HCONST_10 : HCONST_11};
#if D_EXP == 16
                do {
                    Uint32 x, y;
                    x = ctx.i < ctx.n ? BIG_DIGIT(ctx.ptr, ctx.i++) : 0;
                    x += (Uint32)(ctx.i < ctx.n ? BIG_DIGIT(ctx.ptr, ctx.i++) : 0) << 16;
                    y = ctx.i < ctx.n ? BIG_DIGIT(ctx.ptr, ctx.i++) : 0;
                    y += (Uint32)(ctx.i < ctx.n ? BIG_DIGIT(ctx.ptr, ctx.i++) : 0) << 16;
                    UINT32_HASH_2(x, y, ctx.con);
                    TRAP_LOCATION(neg_big_subtag);
                } while (ctx.i < ctx.n);
#elif D_EXP == 32
                do {
                    Uint32 x, y;
                    x = ctx.i < ctx.n ? BIG_DIGIT(ctx.ptr, ctx.i++) : 0;
                    y = ctx.i < ctx.n ? BIG_DIGIT(ctx.ptr, ctx.i++) : 0;
                    UINT32_HASH_2(x, y, ctx.con);
                    TRAP_LOCATION(neg_big_subtag);
                } while (ctx.i < ctx.n);
#elif D_EXP == 64
                do {
                    Uint t;
                    Uint32 x, y;
                    ASSERT(ctx.i < ctx.n);
                    t = BIG_DIGIT(ctx.ptr, ctx.i++);
                    x = t & 0xffffffff;
                    y = t >> 32;
                    UINT32_HASH_2(x, y, ctx.con);
                    TRAP_LOCATION(neg_big_subtag);
                } while (ctx.i < ctx.n);
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
            case EXTERNAL_PORT_SUBTAG: {
                Uint64 number = external_port_number(term);
                Uint32 low = (Uint32) (number & 0xffffffff);
                Uint32 high = (Uint32) ((number >> 32) & 0xffffffff);
                UINT32_HASH_2(low, high, HCONST_6);
                goto hash2_common;
            }
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
            case _TAG_IMMED1_PORT: {
                Uint64 number = internal_port_number(term);
                Uint32 low = (Uint32) (number & 0xffffffff);
                Uint32 high = (Uint32) ((number >> 32) & 0xffffffff);
                UINT32_HASH_2(low, high, HCONST_6);
                goto hash2_common;
            }
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
                  Sint small = signed_val(term);
                  if (SMALL_BITS > 28 && !IS_SSMALL28(small)) {
                      NOT_SSMALL28_HASH(small);
                  } else {
                      SINT32_HASH(small, HCONST);
                  }

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
                if (can_trap) {
                    BUMP_REDS(p, (max_iterations - iterations_until_trap) / ITERATIONS_PER_RED);
                    ASSERT(!(p->flags & F_DISABLE_GC));
                }
                return hash;
            }

            term = ESTACK_POP(s);

            switch (term) {
                case HASH_MAP_TAIL: {
                    hash = (Uint32) ESTACK_POP(s);
                    UINT32_HASH(hash_xor_pairs, HCONST_19);
                    hash_xor_pairs = (Uint32) ESTACK_POP(s);
                    TRAP_LOCATION_NO_CTX(hash2_common_1);
                    goto hash2_common;
                }
                case HASH_MAP_PAIR:
                    hash_xor_pairs ^= hash;
                    hash = 0;
                    TRAP_LOCATION_NO_CTX(hash2_common_2);
                    goto hash2_common;
                default:
                    break;
            }

        }
        TRAP_LOCATION_NO_CTX(hash2_common_3);
    }
    }
#undef TRAP_LOCATION_NO_RED
#undef TRAP_LOCATION
#undef TRAP_LOCATION_NO_CTX
}

Uint32
make_hash2(Eterm term)
{
    return make_hash2_helper(term, 0, NULL, NULL);
}

Uint32
trapping_make_hash2(Eterm term, Eterm* state_mref_write_back, Process* p)
{
    return make_hash2_helper(term, 1, state_mref_write_back, p);
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
 * The property "EVERY BIT of the term that is significant for equality
 * MUST BE USED AS INPUT FOR THE HASH" is nice but no longer crucial for the
 * hashmap implementation that now uses collision nodes at the bottom of
 * the HAMT when all hash bits are exhausted.
 *
 */

/* Use a better mixing function if available. */
#if defined(ERL_INTERNAL_HASH_CRC32C)
#   undef MIX
#   if defined(__x86_64__)
#       define MIX(a,b,c)                                                     \
            do {                                                              \
                Uint32 initial_hash = c;                                      \
                c = __builtin_ia32_crc32si(c, a);                             \
                c = __builtin_ia32_crc32si(c + initial_hash, b);              \
            } while(0)
#   elif defined(__aarch64__)
#       define MIX(a,b,c)                                                     \
            do {                                                              \
                Uint32 initial_hash = c;                                      \
                c = __crc32cw(c, a);                                          \
                c = __crc32cw(c + initial_hash, b);                           \
            } while(0)
#   else
#   error "No suitable CRC32 intrinsic available."
#   endif
#endif

#define CONST_HASH(AConst)                                                    \
    do {  /* Lightweight mixing of constant (type info) */                    \
        hash ^= AConst;                                                       \
        hash = (hash << 17) ^ (hash >> (32-17));                              \
    } while (0)

/*
 * Start with salt, 32-bit prime number, to avoid getting same hash as phash2
 * which can cause bad hashing in distributed ETS tables for example.
 */
#define INTERNAL_HASH_SALT 3432918353U

Uint32
make_internal_hash(Eterm term, Uint32 salt)
{
    Uint32 hash = salt ^ INTERNAL_HASH_SALT;

    /* Optimization. Simple cases before declaration of estack. */
    if (primary_tag(term) == TAG_PRIMARY_IMMED1) {
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

                /*
                 * We rely on key-value iteration order being constant
                 * for identical maps (in this VM instance).
                 */
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

                    for (i = size - 1; i >= 0; i--) {
                        ESTACK_PUSH(s, vs[i]);
                        ESTACK_PUSH(s, ks[i]);
                    }
                    goto pop_next;
                }
                case HAMT_SUBTAG_HEAD_ARRAY:
                case HAMT_SUBTAG_HEAD_BITMAP:
                    size = *ptr++;
                    UINT32_HASH(size, HCONST_16);
                    if (size == 0)
                        goto pop_next;
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
                        ESTACK_PUSH(s, CDR(cons));
                        ESTACK_PUSH(s, CAR(cons));
                    }
                    else {
                        ASSERT(is_boxed(*ptr));
                        /* no special treatment of collision nodes needed,
                           hash them as the tuples they are */
                        ESTACK_PUSH(s, *ptr);
                    }
                    i--; ptr++;
                }
                goto pop_next;
            }
            break;
            case FUN_SUBTAG:
            {
                ErlFunThing* funp = (ErlFunThing *) fun_val(term);

                if (is_local_fun(funp)) {
                    ErlFunEntry* fe = funp->entry.fun;
                    Uint num_free = funp->num_free;
                    UINT32_HASH_2(num_free, fe->module, HCONST_20);
                    UINT32_HASH_2(fe->index, fe->old_uniq, HCONST_21);
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
                } else {
                    ASSERT(is_external_fun(funp) && funp->next == NULL);

                    /* Assumes Export entries never move */
                    POINTER_HASH(funp->entry.exp, HCONST_14);
                    goto pop_next;
                }
            }
            break;
            case REFC_BINARY_SUBTAG:
            case HEAP_BINARY_SUBTAG:
            case SUB_BINARY_SUBTAG:
            {
                byte* bptr;
                Uint sz = binary_size(term);
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
            case REF_SUBTAG: {
                Uint32 *numbers = internal_ref_numbers(term);
                ASSERT(internal_ref_no_numbers(term) >= 3);
                UINT32_HASH(numbers[0], HCONST_7);
                UINT32_HASH_2(numbers[1], numbers[2], HCONST_8);
                if (is_internal_pid_ref(term)) {
#ifdef ARCH_64
                    ASSERT(internal_ref_no_numbers(term) == 5);
                    UINT32_HASH_2(numbers[3], numbers[4], HCONST_9);
#else
                    ASSERT(internal_ref_no_numbers(term) == 4);
                    UINT32_HASH(numbers[3], HCONST_9);
#endif
                }
                goto pop_next;
            }
            case EXTERNAL_REF_SUBTAG:
            {
                ExternalThing* thing = external_thing_ptr(term);
                Uint n = external_thing_ref_no_numbers(thing);
                Uint32 *numbers = external_thing_ref_numbers(thing);

                /* Can contain 0 to 5 32-bit numbers... */

                /* See limitation #2 */
                switch (n) {
                case 5: {
                    Uint32 num4 = numbers[4];
                    if (0) {
                    case 4:
                        num4 = 0;
                        /* Fall through... */
                    }
                    UINT32_HASH_2(numbers[3], num4, HCONST_9);
                    /* Fall through... */
                }
                case 3: {
                    Uint32 num2 = numbers[2];
                    if (0) {
                    case 2:
                        num2 = 0;
                        /* Fall through... */
                    }
                    UINT32_HASH_2(numbers[1], num2, HCONST_8);
                    /* Fall through... */
                }
                case 1:
#ifdef ARCH_64
                    POINTER_HASH(thing->node, HCONST_7);
                    UINT32_HASH(numbers[0], HCONST_7);
#else
                    UINT32_HASH_2(thing->node, numbers[0], HCONST_7);
#endif
                    break;
                case 0:
                    POINTER_HASH(thing->node, HCONST_7);
                    break;
                default:
                    ASSERT(!"Invalid amount of external reference numbers");
                    break;
                }
                goto pop_next;
            }
            case EXTERNAL_PID_SUBTAG: {
                ExternalThing* thing = external_thing_ptr(term);
                /* See limitation #2 */
                POINTER_HASH(thing->node, HCONST_5);
                UINT32_HASH_2(thing->data.pid.num, thing->data.pid.ser, HCONST_5);
                goto pop_next;
            }
            case EXTERNAL_PORT_SUBTAG: {
                ExternalThing* thing = external_thing_ptr(term);
                /* See limitation #2 */
                POINTER_HASH(thing->node, HCONST_6);
                UINT32_HASH_2(thing->data.ui32[0], thing->data.ui32[1], HCONST_6);
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
                erts_exit(ERTS_ERROR_EXIT, "Invalid tag in make_internal_hash(0x%X, %lu)\n", term, salt);
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
            erts_exit(ERTS_ERROR_EXIT, "Invalid tag in make_internal_hash(0x%X, %lu)\n", term, salt);

        pop_next:
            if (ESTACK_ISEMPTY(s)) {
                DESTROY_ESTACK(s);

                return hash;
            }

            term = ESTACK_POP(s);

            switch (term) {
                case HASH_CDR:
                    CONST_HASH(HCONST_18);   /* Hash CDR i cons cell */
                    goto pop_next;
                default:
                    break;
            }
        }
    }
    }

}

#ifdef DBG_HASHMAP_COLLISION_BONANZA
Uint32 erts_dbg_hashmap_collision_bonanza(Uint32 hash, Eterm key)
{
/*{
        static Uint32 hashvec[7] = {
            0x02345678,
            0x12345678,
            0xe2345678,
            0xf2345678,
            0x12abcdef,
            0x13abcdef,
            0xcafebabe
        };
        hash = hashvec[hash % (sizeof(hashvec) / sizeof(hashvec[0]))];
    }*/
    const Uint32 bad_hash = (hash & 0x12482481) * 1442968193;
    const Uint32 bad_bits = hash % 67;
    if (bad_bits < 32) {
        /* Mix in a number of high good bits to get "randomly" close
           to the collision nodes */
        const Uint32 bad_mask = (1 << bad_bits) - 1;
        return (hash & ~bad_mask) | (bad_hash & bad_mask);
    }
    return bad_hash;
}
#endif

/* Term hash function for hashmaps */
Uint32 make_map_hash(Eterm key) {
    Uint32 hash;

    hash = make_internal_hash(key, 0);

#ifdef DBG_HASHMAP_COLLISION_BONANZA
    hash = erts_dbg_hashmap_collision_bonanza(hash, key);
#endif
    return hash;
}

#undef CONST_HASH
#undef HASH_MAP_TAIL
#undef HASH_MAP_PAIR
#undef HASH_CDR

#undef UINT32_HASH_2
#undef UINT32_HASH
#undef SINT32_HASH

#undef HCONST
#undef MIX
