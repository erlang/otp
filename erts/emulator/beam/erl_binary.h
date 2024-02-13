/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2024. All Rights Reserved.
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

enum binary_flags {
    BIN_FLAG_MAGIC =            (1 << 0),
    BIN_FLAG_DRV =              (1 << 1),

    /* Whether the binary is writable and/or actively written to. These are
     * valid iff the reference count is 1: any kind of observation, including
     * copying to another process, must clear these flags. */
    BIN_FLAG_WRITABLE =         (1 << 2),
    BIN_FLAG_ACTIVE_WRITER =    (1 << 3),
};

#define ERTS_BINARY_STRUCT_ALIGNMENT

/* Add fields in binary_internals, otherwise the drivers crash */
struct binary_internals {
    UWord flags;
    /* Valid iff one of BIN_FLAG_WRITABLE and BIN_FLAG_MAGIC is set. */
    UWord apparent_size;
    erts_refc_t refc;

    ERTS_BINARY_STRUCT_ALIGNMENT
};

typedef struct binary {
    struct binary_internals intern;
    SWord orig_size;

    /* Note that this field has to be 8-byte aligned even on 32-bit
     * platforms. Any required padding should be added at the tail of
     * the binary_internals struct above. */
    char orig_bytes[1]; /* to be continued */
} Binary;

#define ERTS_SIZEOF_Binary(Sz) \
    (offsetof(Binary,orig_bytes) + (Sz))

#if ERTS_REF_NUMBERS != 3
#error "Update ErtsMagicBinary"
#endif

#ifdef ARCH_32
 /* *DO NOT USE* only for alignment. */
#define ERTS_MAGIC_BINARY_STRUCT_ALIGNMENT Uint32 align__;
#else
#define ERTS_MAGIC_BINARY_STRUCT_ALIGNMENT
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
            ERTS_MAGIC_BINARY_STRUCT_ALIGNMENT
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

#endif /* ERL_BINARY_H__TYPES__ */

#if !defined(ERL_BINARY_H__) && !defined(ERTS_BINARY_TYPES_ONLY__)
#define ERL_BINARY_H__

#include "erl_threads.h"
#include "bif.h"
#include "erl_bif_unique.h"
#include "erl_bits.h"

void erts_init_binary(void);

/* Used by unicode module */
Eterm erts_bin_bytes_to_list(Eterm previous, Eterm* hp, const byte* bytes, Uint size, Uint bitoffs);

/*
 * Common implementation for erlang:list_to_binary/1 and binary:list_to_bin/1
 */

BIF_RETTYPE erts_list_to_binary_bif(Process *p, Eterm arg, Export *bif);
BIF_RETTYPE erts_binary_part(Process *p, Eterm binary, Eterm epos, Eterm elen);


typedef struct {
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

ERTS_GLB_INLINE Binary *erts_bin_drv_alloc_fnf(Uint size);
ERTS_GLB_INLINE Binary *erts_bin_drv_alloc(Uint size);
ERTS_GLB_INLINE Binary *erts_bin_nrml_alloc_fnf(Uint size);
ERTS_GLB_INLINE Binary *erts_bin_nrml_alloc(Uint size);
ERTS_GLB_INLINE Binary *erts_bin_realloc_fnf(Binary *bp, Uint size);
ERTS_GLB_INLINE Binary *erts_bin_realloc(Binary *bp, Uint size);
ERTS_GLB_INLINE void erts_magic_binary_free(Binary *bp);
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

/* A binary's size in bits must fit into a word for matching to work. We used
 * to allow creating larger binaries than this, but they acted really strangely
 * in Erlang code and were pretty much only usable in drivers and NIFs.
 *
 * This check also ensures, indirectly, that there won't be an overflow when
 * the size is bumped by CHICKEN_PAD and the binary struct itself. */
#define IS_BINARY_SIZE_OK(BYTE_SIZE) \
    ERTS_LIKELY(BYTE_SIZE <= ERTS_UWORD_MAX / CHAR_BIT)


#if ERTS_GLB_INLINE_INCL_FUNC_DEF

#include <stddef.h> /* offsetof */

/* Explicit extra bytes allocated to counter buggy drivers.
** These extra bytes where earlier (< R13B04) added by an alignment-bug
** in this code. Do we dare remove this in some major release (R14?) maybe?
*/
#if defined(DEBUG) || defined(VALGRIND) || defined(ADDRESS_SANITIZER)
#  define CHICKEN_PAD 0
#else
#  define CHICKEN_PAD (sizeof(void*) - 1)
#endif

ERTS_GLB_INLINE Binary *
erts_bin_drv_alloc_fnf(Uint size)
{
    Binary *res;
    Uint bsize;

    if (!IS_BINARY_SIZE_OK(size))
        return NULL;
    bsize = ERTS_SIZEOF_Binary(size) + CHICKEN_PAD;

    res = (Binary *)erts_alloc_fnf(ERTS_ALC_T_DRV_BINARY, bsize);
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
    Binary *res = erts_bin_drv_alloc_fnf(size);

    if (res) {
        return res;
    }

    erts_alloc_enomem(ERTS_ALC_T_DRV_BINARY, size);
}

ERTS_GLB_INLINE Binary *
erts_bin_nrml_alloc_fnf(Uint size)
{
    Binary *res;
    Uint bsize;

    if (!IS_BINARY_SIZE_OK(size))
        return NULL;
    bsize = ERTS_SIZEOF_Binary(size);

    res = (Binary *)erts_alloc_fnf(ERTS_ALC_T_BINARY, bsize);
    ERTS_CHK_BIN_ALIGNMENT(res);

    if (res) {
        res->orig_size = size;
        res->intern.flags = 0;
        erts_refc_init(&res->intern.refc, 1);
    }

    return res;
}

ERTS_GLB_INLINE Binary *
erts_bin_nrml_alloc(Uint size)
{
    Binary *res = erts_bin_nrml_alloc_fnf(size);

    if (res) {
        return res;
    }

    erts_alloc_enomem(ERTS_ALC_T_BINARY, size);
}

ERTS_GLB_INLINE Binary *
erts_bin_realloc_fnf(Binary *bp, Uint size)
{
    ErtsAlcType_t type;
    Binary *nbp;
    Uint bsize;
    
    ASSERT((bp->intern.flags & BIN_FLAG_MAGIC) == 0);
    if (!IS_BINARY_SIZE_OK(size))
        return NULL;
    bsize = ERTS_SIZEOF_Binary(size);

    if (bp->intern.flags & BIN_FLAG_DRV) {
        type = ERTS_ALC_T_DRV_BINARY;
        bsize += CHICKEN_PAD;
    }
    else {
        type = ERTS_ALC_T_BINARY;
    }

    nbp = (Binary *)erts_realloc_fnf(type, (void *) bp, bsize);
    ERTS_CHK_BIN_ALIGNMENT(nbp);

    if (nbp) {
        nbp->orig_size = size;
    }

    return nbp;
}

ERTS_GLB_INLINE Binary *
erts_bin_realloc(Binary *bp, Uint size)
{
    Binary *nbp = erts_bin_realloc_fnf(bp, size);

    if (nbp) {
        return nbp;
    }

    if (bp->intern.flags & BIN_FLAG_DRV) {
        erts_realloc_enomem(ERTS_ALC_T_DRV_BINARY, bp, size);
    } else {
        erts_realloc_enomem(ERTS_ALC_T_BINARY, bp, size);
    }
}

ERTS_GLB_INLINE void
erts_magic_binary_free(Binary *bp)
{
    erts_magic_ref_remove_bin(ERTS_MAGIC_BIN_REFN(bp));
    erts_free(ERTS_MAGIC_BIN_ATYPE(bp), (void *) bp);
}

ERTS_GLB_INLINE void
erts_bin_free(Binary *bp)
{
    if (bp->intern.flags & BIN_FLAG_MAGIC) {
        if (!ERTS_MAGIC_BIN_DESTRUCTOR(bp)(bp)) {
            /* Destructor took control of the deallocation */
            return;
        }
        erts_magic_binary_free(bp);
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
    Binary* bptr = (Binary *)erts_alloc_fnf(alloc_type, bsize);
    ASSERT(bsize > size);
    if (!bptr)
	erts_alloc_n_enomem(ERTS_ALC_T2N(alloc_type), bsize);
    ERTS_CHK_BIN_ALIGNMENT(bptr);
    bptr->intern.flags = BIN_FLAG_MAGIC;
    bptr->intern.apparent_size = size;
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
    mip = (ErtsMagicIndirectionWord*)ERTS_MAGIC_BIN_UNALIGNED_DATA(bp);
    return &mip->atomic_word;
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* !ERL_BINARY_H__ */
