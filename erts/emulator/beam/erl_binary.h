/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2017. All Rights Reserved.
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

#ifndef ERL_BINARY_H__TYPES__
#define ERL_BINARY_H__TYPES__

/*
** Just like the driver binary but with initial flags
** Note that the two structures Binary and ErlDrvBinary HAVE to
** be equal except for extra fields in the beginning of the struct.
** ErlDrvBinary is defined in erl_driver.h.
** When driver_alloc_binary is called, a Binary is allocated, but 
** the pointer returned is to the address of the first element that
** also occurs in the ErlDrvBinary struct (driver.*binary takes care if this).
** The driver need never know about additions to the internal Binary of the
** emulator. One should however NEVER be sloppy when mixing ErlDrvBinary
** and Binary, the macros below can convert one type to the other, as they both
** in reality are equal.
*/

#ifdef ARCH_32
 /* *DO NOT USE* only for alignment. */
#define ERTS_BINARY_STRUCT_ALIGNMENT Uint32 align__;
#else
#define ERTS_BINARY_STRUCT_ALIGNMENT
#endif

/* Add fields in binary_internals, otherwise the drivers crash */
struct binary_internals {
    UWord flags;
    erts_refc_t refc;
    ERTS_BINARY_STRUCT_ALIGNMENT
};


typedef struct binary {
    struct binary_internals intern;
    SWord orig_size;
    char orig_bytes[1]; /* to be continued */
} Binary;

#define ERTS_SIZEOF_Binary(Sz) \
    (offsetof(Binary,orig_bytes) + (Sz))

#if ERTS_REF_NUMBERS != 3
#error "Update ErtsMagicBinary"
#endif

typedef struct magic_binary ErtsMagicBinary;
struct magic_binary {
    struct binary_internals intern;
    SWord orig_size;
    int (*destructor)(Binary *);
    Uint32 refn[ERTS_REF_NUMBERS];
    ErtsAlcType_t alloc_type;
    union {
        struct {
            ERTS_BINARY_STRUCT_ALIGNMENT
            char data[1];
        } aligned;
        struct {
            char data[1];
        } unaligned;
    } u;
};

#define ERTS_MAGIC_BIN_BYTES_TO_ALIGN \
    (offsetof(ErtsMagicBinary,u.aligned.data) - \
     offsetof(ErtsMagicBinary,u.unaligned.data))

typedef union {
    Binary binary;
    ErtsMagicBinary magic_binary;
    struct {
	struct binary_internals intern;
	ErlDrvBinary binary;
    } driver;
} ErtsBinary;

/*
 * 'Binary' alignment:
 *   Address of orig_bytes[0] of a Binary should always be 8-byte aligned.
 * It is assumed that the flags, refc, and orig_size fields are 4 bytes on
 * 32-bits architectures and 8 bytes on 64-bits architectures.
 */

#define ERTS_MAGIC_BIN_REFN(BP) \
  ((ErtsBinary *) (BP))->magic_binary.refn
#define ERTS_MAGIC_BIN_ATYPE(BP) \
  ((ErtsBinary *) (BP))->magic_binary.alloc_type
#define ERTS_MAGIC_DATA_OFFSET \
  (offsetof(ErtsMagicBinary,u.aligned.data) - offsetof(Binary,orig_bytes))
#define ERTS_MAGIC_BIN_DESTRUCTOR(BP) \
  ((ErtsBinary *) (BP))->magic_binary.destructor
#define ERTS_MAGIC_BIN_DATA(BP) \
  ((void *) ((ErtsBinary *) (BP))->magic_binary.u.aligned.data)
#define ERTS_MAGIC_BIN_DATA_SIZE(BP) \
  ((BP)->orig_size - ERTS_MAGIC_DATA_OFFSET)
#define ERTS_MAGIC_DATA_OFFSET \
  (offsetof(ErtsMagicBinary,u.aligned.data) - offsetof(Binary,orig_bytes))
#define ERTS_MAGIC_BIN_ORIG_SIZE(Sz) \
  (ERTS_MAGIC_DATA_OFFSET + (Sz))
#define ERTS_MAGIC_BIN_SIZE(Sz) \
  (offsetof(ErtsMagicBinary,u.aligned.data) + (Sz))
#define ERTS_MAGIC_BIN_FROM_DATA(DATA) \
  ((ErtsBinary*)((char*)(DATA) - offsetof(ErtsMagicBinary,u.aligned.data)))

/* On 32-bit arch these macro variants will save memory
   by not forcing 8-byte alignment for the magic payload.
*/
#define ERTS_MAGIC_BIN_UNALIGNED_DATA(BP) \
  ((void *) ((ErtsBinary *) (BP))->magic_binary.u.unaligned.data)
#define ERTS_MAGIC_UNALIGNED_DATA_OFFSET \
  (offsetof(ErtsMagicBinary,u.unaligned.data) - offsetof(Binary,orig_bytes))
#define ERTS_MAGIC_BIN_UNALIGNED_DATA_SIZE(BP) \
  ((BP)->orig_size - ERTS_MAGIC_UNALIGNED_DATA_OFFSET)
#define ERTS_MAGIC_BIN_UNALIGNED_ORIG_SIZE(Sz) \
  (ERTS_MAGIC_UNALIGNED_DATA_OFFSET + (Sz))
#define ERTS_MAGIC_BIN_UNALIGNED_SIZE(Sz) \
  (offsetof(ErtsMagicBinary,u.unaligned.data) + (Sz))
#define ERTS_MAGIC_BIN_FROM_UNALIGNED_DATA(DATA) \
  ((ErtsBinary*)((char*)(DATA) - offsetof(ErtsMagicBinary,u.unaligned.data)))


#define Binary2ErlDrvBinary(B) (&((ErtsBinary *) (B))->driver.binary)
#define ErlDrvBinary2Binary(D) ((Binary *) \
				(((char *) (D)) \
				 - offsetof(ErtsBinary, driver.binary)))

/* A "magic" binary flag */
#define BIN_FLAG_MAGIC      1
#define BIN_FLAG_USR1       2 /* Reserved for use by different modules too mark */
#define BIN_FLAG_USR2       4 /*  certain binaries as special (used by ets) */
#define BIN_FLAG_DRV        8

#endif /* ERL_BINARY_H__TYPES__ */

#if !defined(ERL_BINARY_H__) && !defined(ERTS_BINARY_TYPES_ONLY__)
#define ERL_BINARY_H__

#include "erl_threads.h"
#include "bif.h"
#include "erl_bif_unique.h"
#include "erl_bits.h"

/*
 * Maximum number of bytes to place in a heap binary.
 */

#define ERL_ONHEAP_BIN_LIMIT 64

#define ERL_SUB_BIN_SIZE (sizeof(ErlSubBin)/sizeof(Eterm))
#define HEADER_SUB_BIN	_make_header(ERL_SUB_BIN_SIZE-2,_TAG_HEADER_SUB_BIN)

/*
 * This structure represents a HEAP_BINARY.
 */

typedef struct erl_heap_bin {
    Eterm thing_word;		/* Subtag HEAP_BINARY_SUBTAG. */
    Uint size;			/* Binary size in bytes. */
    Eterm data[1];		/* The data in the binary. */
} ErlHeapBin;

#define heap_bin_size(num_bytes)		\
  (sizeof(ErlHeapBin)/sizeof(Eterm) - 1 +	\
   ((num_bytes)+sizeof(Eterm)-1)/sizeof(Eterm))

#define header_heap_bin(num_bytes) \
  _make_header(heap_bin_size(num_bytes)-1,_TAG_HEADER_HEAP_BIN)

/*
 * Get the size in bytes of any type of binary.
 */

#define binary_size(Bin) (binary_val(Bin)[1])

#define binary_bitsize(Bin)			\
  ((*binary_val(Bin) == HEADER_SUB_BIN) ?	\
   ((ErlSubBin *) binary_val(Bin))->bitsize:	\
   0)

#define binary_bitoffset(Bin)			\
  ((*binary_val(Bin) == HEADER_SUB_BIN) ?	\
   ((ErlSubBin *) binary_val(Bin))->bitoffs:	\
   0)

/*
 * Get the pointer to the actual data bytes in a binary.
 * Works for any type of binary. Always use binary_bytes() if
 * you know that the binary cannot be a sub binary.
 *
 * Bin: input variable (Eterm)
 * Bytep: output variable (byte *)
 * Bitoffs: output variable (Uint)
 * Bitsize: output variable (Uint)
 */

#define ERTS_GET_BINARY_BYTES(Bin,Bytep,Bitoffs,Bitsize)                \
do {									\
    Eterm* _real_bin = binary_val(Bin);		                	\
    Uint _offs = 0;							\
    Bitoffs = Bitsize = 0;						\
    if (*_real_bin == HEADER_SUB_BIN) {					\
	ErlSubBin* _sb = (ErlSubBin *) _real_bin;			\
	_offs = _sb->offs;						\
        Bitoffs = _sb->bitoffs;						\
        Bitsize = _sb->bitsize;						\
	_real_bin = binary_val(_sb->orig);	        		\
    }									\
    if (*_real_bin == HEADER_PROC_BIN) {				\
	Bytep = ((ProcBin *) _real_bin)->bytes + _offs;			\
    } else {								\
	Bytep = (byte *)(&(((ErlHeapBin *) _real_bin)->data)) + _offs;	\
    }									\
} while (0)

/*
 * Get the real binary from any binary type, where "real" means
 * a REFC or HEAP binary. Also get the byte and bit offset into the
 * real binary. Useful if you want to build a SUB binary from
 * any binary.
 *
 * Bin: Input variable (Eterm)
 * RealBin: Output variable (Eterm)
 * ByteOffset: Output variable (Uint)
 * BitOffset: Offset in bits (Uint)
 * BitSize: Extra bit size (Uint)
 */

#define ERTS_GET_REAL_BIN(Bin, RealBin, ByteOffset, BitOffset, BitSize) \
  do {									\
    ErlSubBin* _sb = (ErlSubBin *) binary_val(Bin);	                \
    if (_sb->thing_word == HEADER_SUB_BIN) {				\
      RealBin = _sb->orig;						\
      ByteOffset = _sb->offs;						\
      BitOffset = _sb->bitoffs;						\
      BitSize = _sb->bitsize;						\
    } else {								\
      RealBin = Bin;							\
      ByteOffset = BitOffset = BitSize = 0;				\
    }									\
  } while (0)

/*
 * Get a pointer to the binary bytes, for a heap or refc binary
 * (NOT sub binary).
 */
#define binary_bytes(Bin)						\
  (*binary_val(Bin) == HEADER_PROC_BIN ?				\
   ((ProcBin *) binary_val(Bin))->bytes :				\
   (ASSERT(thing_subtag(*binary_val(Bin)) == HEAP_BINARY_SUBTAG),	\
   (byte *)(&(((ErlHeapBin *) binary_val(Bin))->data))))

void erts_init_binary(void);

byte* erts_get_aligned_binary_bytes_extra(Eterm, byte**, ErtsAlcType_t, unsigned extra);
/* Used by unicode module */
Eterm erts_bin_bytes_to_list(Eterm previous, Eterm* hp, byte* bytes, Uint size, Uint bitoffs);

/*
 * Common implementation for erlang:list_to_binary/1 and binary:list_to_bin/1
 */

BIF_RETTYPE erts_list_to_binary_bif(Process *p, Eterm arg, Export *bif);
BIF_RETTYPE erts_gc_binary_part(Process *p, Eterm *reg, Eterm live, int range_is_tuple);
BIF_RETTYPE erts_binary_part(Process *p, Eterm binary, Eterm epos, Eterm elen);


typedef union {
    /*
     * These two are almost always of
     * the same size, but when fallback
     * atomics are used they might
     * differ in size.
     */
    erts_atomic_t smp_atomic_word;
    erts_atomic_t atomic_word;
} ErtsMagicIndirectionWord;

#if defined(__i386__) || !defined(__GNUC__)
/*
 * Doubles aren't required to be 8-byte aligned on intel x86.
 * (if not gnuc we don't know if __i386__ is defined on x86;
 *  therefore, assume intel x86...)
 */
#  define ERTS_BIN_ALIGNMENT_MASK ((Uint) 3)
#else
#  define ERTS_BIN_ALIGNMENT_MASK ((Uint) 7)
#endif

#define ERTS_CHK_BIN_ALIGNMENT(B) \
  do { ASSERT(!(B) || (((UWord) &((Binary *)(B))->orig_bytes[0]) & ERTS_BIN_ALIGNMENT_MASK) == ((UWord) 0)); } while(0)

ERTS_GLB_INLINE byte* erts_get_aligned_binary_bytes(Eterm bin, byte** base_ptr);
ERTS_GLB_INLINE void erts_free_aligned_binary_bytes(byte* buf);
ERTS_GLB_INLINE void erts_free_aligned_binary_bytes_extra(byte* buf, ErtsAlcType_t);
ERTS_GLB_INLINE Binary *erts_bin_drv_alloc_fnf(Uint size);
ERTS_GLB_INLINE Binary *erts_bin_drv_alloc(Uint size);
ERTS_GLB_INLINE Binary *erts_bin_nrml_alloc(Uint size);
ERTS_GLB_INLINE Binary *erts_bin_realloc_fnf(Binary *bp, Uint size);
ERTS_GLB_INLINE Binary *erts_bin_realloc(Binary *bp, Uint size);
ERTS_GLB_INLINE void erts_bin_free(Binary *bp);
ERTS_GLB_INLINE void erts_bin_release(Binary *bp);
ERTS_GLB_INLINE Binary *erts_create_magic_binary_x(Uint size,
                                                  int (*destructor)(Binary *),
                                                   ErtsAlcType_t alloc_type,
                                                  int unaligned);
ERTS_GLB_INLINE Binary *erts_create_magic_binary(Uint size,
						 int (*destructor)(Binary *));
ERTS_GLB_INLINE Binary *erts_create_magic_indirection(int (*destructor)(Binary *));
ERTS_GLB_INLINE erts_atomic_t *erts_binary_to_magic_indirection(Binary *bp);
ERTS_GLB_INLINE erts_atomic_t *erts_binary_to_magic_indirection(Binary *bp);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

#include <stddef.h> /* offsetof */

ERTS_GLB_INLINE byte*
erts_get_aligned_binary_bytes(Eterm bin, byte** base_ptr)
{
    return erts_get_aligned_binary_bytes_extra(bin, base_ptr, ERTS_ALC_T_TMP, 0);
}

ERTS_GLB_INLINE void
erts_free_aligned_binary_bytes_extra(byte* buf, ErtsAlcType_t allocator)
{
    if (buf) {
	erts_free(allocator, (void *) buf);
    }
}

ERTS_GLB_INLINE void
erts_free_aligned_binary_bytes(byte* buf)
{
    erts_free_aligned_binary_bytes_extra(buf,ERTS_ALC_T_TMP);
}

/* Explicit extra bytes allocated to counter buggy drivers.
** These extra bytes where earlier (< R13B04) added by an alignment-bug
** in this code. Do we dare remove this in some major release (R14?) maybe?
*/
#if defined(DEBUG) || defined(VALGRIND)
#  define CHICKEN_PAD 0
#else
#  define CHICKEN_PAD (sizeof(void*) - 1)
#endif

ERTS_GLB_INLINE Binary *
erts_bin_drv_alloc_fnf(Uint size)
{
    Uint bsize = ERTS_SIZEOF_Binary(size) + CHICKEN_PAD;
    Binary *res;

    if (bsize < size) /* overflow */
	return NULL;
    res = erts_alloc_fnf(ERTS_ALC_T_DRV_BINARY, bsize);
    ERTS_CHK_BIN_ALIGNMENT(res);
    if (res) {
	res->orig_size = size;
	res->intern.flags = BIN_FLAG_DRV;
        erts_refc_init(&res->intern.refc, 1);
    }
    return res;
}

ERTS_GLB_INLINE Binary *
erts_bin_drv_alloc(Uint size)
{
    Uint bsize = ERTS_SIZEOF_Binary(size) + CHICKEN_PAD;
    Binary *res;

    if (bsize < size) /* overflow */
	erts_alloc_enomem(ERTS_ALC_T_DRV_BINARY, size);
    res = erts_alloc(ERTS_ALC_T_DRV_BINARY, bsize);
    ERTS_CHK_BIN_ALIGNMENT(res);
    res->orig_size = size;
    res->intern.flags = BIN_FLAG_DRV;
    erts_refc_init(&res->intern.refc, 1);
    return res;
}

ERTS_GLB_INLINE Binary *
erts_bin_nrml_alloc(Uint size)
{
    Uint bsize = ERTS_SIZEOF_Binary(size) + CHICKEN_PAD;
    Binary *res;

    if (bsize < size) /* overflow */
	erts_alloc_enomem(ERTS_ALC_T_BINARY, size);
    res = erts_alloc(ERTS_ALC_T_BINARY, bsize);
    ERTS_CHK_BIN_ALIGNMENT(res);
    res->orig_size = size;
    res->intern.flags = 0;
    erts_refc_init(&res->intern.refc, 1);
    return res;
}

ERTS_GLB_INLINE Binary *
erts_bin_realloc_fnf(Binary *bp, Uint size)
{
    Binary *nbp;
    Uint bsize = ERTS_SIZEOF_Binary(size) + CHICKEN_PAD;
    ErtsAlcType_t type = (bp->intern.flags & BIN_FLAG_DRV) ? ERTS_ALC_T_DRV_BINARY
	                                            : ERTS_ALC_T_BINARY;
    ASSERT((bp->intern.flags & BIN_FLAG_MAGIC) == 0);
    if (bsize < size) /* overflow */
	return NULL;
    nbp = erts_realloc_fnf(type, (void *) bp, bsize);
    ERTS_CHK_BIN_ALIGNMENT(nbp);
    if (nbp)
	nbp->orig_size = size;
    return nbp;
}

ERTS_GLB_INLINE Binary *
erts_bin_realloc(Binary *bp, Uint size)
{
    Binary *nbp;
    Uint bsize = ERTS_SIZEOF_Binary(size) + CHICKEN_PAD;
    ErtsAlcType_t type = (bp->intern.flags & BIN_FLAG_DRV) ? ERTS_ALC_T_DRV_BINARY
	                                            : ERTS_ALC_T_BINARY;
    ASSERT((bp->intern.flags & BIN_FLAG_MAGIC) == 0);
    if (bsize < size) /* overflow */
	erts_realloc_enomem(type, bp, size);
    nbp = erts_realloc_fnf(type, (void *) bp, bsize);
    if (!nbp)
	erts_realloc_enomem(type, bp, bsize);
    ERTS_CHK_BIN_ALIGNMENT(nbp);
    nbp->orig_size = size;
    return nbp;
}

ERTS_GLB_INLINE void
erts_bin_free(Binary *bp)
{
    if (bp->intern.flags & BIN_FLAG_MAGIC) {
        if (!ERTS_MAGIC_BIN_DESTRUCTOR(bp)(bp)) {
            /* Destructor took control of the deallocation */
            return;
        }
	erts_magic_ref_remove_bin(ERTS_MAGIC_BIN_REFN(bp));
        erts_free(ERTS_MAGIC_BIN_ATYPE(bp), (void *) bp);
    }
    else if (bp->intern.flags & BIN_FLAG_DRV)
	erts_free(ERTS_ALC_T_DRV_BINARY, (void *) bp);
    else
	erts_free(ERTS_ALC_T_BINARY, (void *) bp);
}

ERTS_GLB_INLINE void
erts_bin_release(Binary *bp)
{
    if (erts_refc_dectest(&bp->intern.refc, 0) == 0) {
        erts_bin_free(bp);
    }
}

ERTS_GLB_INLINE Binary *
erts_create_magic_binary_x(Uint size, int (*destructor)(Binary *),
                           ErtsAlcType_t alloc_type,
                           int unaligned)
{
    Uint bsize = unaligned ? ERTS_MAGIC_BIN_UNALIGNED_SIZE(size)
                           : ERTS_MAGIC_BIN_SIZE(size);
    Binary* bptr = erts_alloc_fnf(alloc_type, bsize);
    ASSERT(bsize > size);
    if (!bptr)
	erts_alloc_n_enomem(ERTS_ALC_T2N(alloc_type), bsize);
    ERTS_CHK_BIN_ALIGNMENT(bptr);
    bptr->intern.flags = BIN_FLAG_MAGIC;
    bptr->orig_size = unaligned ? ERTS_MAGIC_BIN_UNALIGNED_ORIG_SIZE(size)
                                : ERTS_MAGIC_BIN_ORIG_SIZE(size);
    erts_refc_init(&bptr->intern.refc, 0);
    ERTS_MAGIC_BIN_DESTRUCTOR(bptr) = destructor;
    ERTS_MAGIC_BIN_ATYPE(bptr) = alloc_type;
    erts_make_magic_ref_in_array(ERTS_MAGIC_BIN_REFN(bptr));
    return bptr;
}

ERTS_GLB_INLINE Binary *
erts_create_magic_binary(Uint size, int (*destructor)(Binary *))
{
    return erts_create_magic_binary_x(size, destructor,
                                      ERTS_ALC_T_BINARY, 0);
}

ERTS_GLB_INLINE Binary *
erts_create_magic_indirection(int (*destructor)(Binary *))
{
    return erts_create_magic_binary_x(sizeof(ErtsMagicIndirectionWord),
                                      destructor,
                                      ERTS_ALC_T_MINDIRECTION,
                                      1); /* Not 64-bit aligned,
                                             but word aligned */
}

ERTS_GLB_INLINE erts_atomic_t *
erts_binary_to_magic_indirection(Binary *bp)
{
    ErtsMagicIndirectionWord *mip;
    ASSERT(bp->intern.flags & BIN_FLAG_MAGIC);
    ASSERT(ERTS_MAGIC_BIN_ATYPE(bp) == ERTS_ALC_T_MINDIRECTION);
    mip = ERTS_MAGIC_BIN_UNALIGNED_DATA(bp);
    return &mip->smp_atomic_word;
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* !ERL_BINARY_H__ */
