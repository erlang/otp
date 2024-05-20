/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2024. All Rights Reserved.
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

#ifndef __ERL_BITS_H__
#define __ERL_BITS_H__

/** @brief returns the number of bytes needed to store \c x bits. */
#define NBYTES(x)  (((Uint64)(x) + (Uint64) 7) >> 3)
/** @brief returns the number of bits there are in \c x bytes. */
#define NBITS(x)  ((Uint64)(x) << 3)

#define BYTE_OFFSET(offset_in_bits) ((Uint)(offset_in_bits) >> 3)
#define BIT_OFFSET(offset_in_bits) ((offset_in_bits) & 7)

/* As the above, but slightly renamed to avoid confusing sizes and offsets. */
#define BYTE_SIZE(size_in_bits) BYTE_OFFSET(size_in_bits)
#define TAIL_BITS(size_in_bits) BIT_OFFSET(size_in_bits)

/* ************************************************************************* */

#define bitstring_size(Bin)                                                   \
    ((bitstring_val(Bin)[0]) == HEADER_SUB_BITS ?                             \
     (((ErlSubBits*)bitstring_val(Bin))->end -                                \
      ((ErlSubBits*)bitstring_val(Bin))->start) :                             \
     (((ErlHeapBits*)bitstring_val(Bin))->size))

/* This structure represents either the term form of an off-heap bitstring, or
 * a bitstring match context. */
typedef struct erl_sub_bits {
    /* Subtag SUB_BITS_SUBTAG. */
    Eterm thing_word;

    /* Combined base pointer and flag field, making room for the flags by
     * truncating the lowest 2 bits from the base pointer and compensating by
     * bumping start/end to match.
     *
     * This means that the base pointer returned from ERTS_GET_BITSTRING and
     * friends is _ONLY_ valid when adjusted by the provided offset.
     *
     * This is placed first to simplify checking whether this is a viable
     * match context. */
    UWord base_flags;

    /* Start and end offset in bits. */
    Uint start;
    Uint end;

    /* Tagged pointer to BinRef or ErlHeapBits, the latter is valid iff this is
     * a match context. */
    Eterm orig;
} ErlSubBits;

#define ERL_SUB_BITS_FLAGS_MATCH_CONTEXT                                      \
    ERL_SUB_BITS_FLAG_MUTABLE
#define ERL_SUB_BITS_FLAGS_WRITABLE                                           \
    (ERL_SUB_BITS_FLAG_MUTABLE | ERL_SUB_BITS_FLAG_VOLATILE)

/* Whether it's safe to modify the SB itself, for example match contexts or
 * active writable binaries (erts_bs_private_append). */
#define ERL_SUB_BITS_FLAG_MUTABLE ((UWord)(1 << 0))
/* Whether the underlying data can move around, for example if the SB is (or
 * was) a writable bitstring. */
#define ERL_SUB_BITS_FLAG_VOLATILE ((UWord)(1 << 1))
#define ERL_SUB_BITS_FLAG_MASK ((UWord)3)

ERTS_GLB_INLINE void
erl_sub_bits_init(ErlSubBits *sb, UWord flags, Eterm orig, const void *base,
                  Uint offset, Uint size);

ERTS_GLB_INLINE void
erl_sub_bits_update_moved(ErlSubBits *sb, Eterm orig);

#define erl_sub_bits_is_normal(SubBits)                                       \
    (erl_sub_bits_get_flags(SubBits) == 0)

#define erl_sub_bits_is_match_context(SubBits)                                \
    (erl_sub_bits_get_flags(SubBits) == ERL_SUB_BITS_FLAG_MUTABLE)

#define erl_sub_bits_is_writable(SubBits)                                     \
    (erl_sub_bits_get_flags(SubBits) ==                                       \
     (ERL_SUB_BITS_FLAG_MUTABLE | ERL_SUB_BITS_FLAG_VOLATILE))

#define erl_sub_bits_was_writable(SubBits)                                    \
    (erl_sub_bits_get_flags(SubBits) & ERL_SUB_BITS_FLAG_VOLATILE)

#define erl_sub_bits_clear_writable(SubBits)                                  \
    (ASSERT(erl_sub_bits_is_writable(SubBits)),                               \
     (SubBits)->base_flags &= ~ERL_SUB_BITS_FLAG_MUTABLE)

#define erl_sub_bits_get_flags(SubBits)                                       \
    ((SubBits)->base_flags & ERL_SUB_BITS_FLAG_MASK)

#define erl_sub_bits_get_base(SubBits)                                        \
    ((byte*)((SubBits)->base_flags & ~ERL_SUB_BITS_FLAG_MASK))

/** @brief The size in words of an ErlSubBits. */
#define ERL_SUB_BITS_SIZE (sizeof(ErlSubBits) / sizeof(Eterm))

#define HEADER_SUB_BITS _make_header(ERL_SUB_BITS_SIZE-1,_TAG_HEADER_SUB_BITS)

/** @brief A handle to an off-heap binary. While terms internally, these can
 * only be referred to by sub-bitstrings, and should never be exposed to the
 * user. */
typedef struct bin_ref {
    Eterm thing_word;           /* Subtag BIN_REF_SUBTAG. */
    Binary *val;                /* Pointer to Binary structure. */
    struct erl_off_heap_header *next;
} BinRef;

#define HEADER_BIN_REF _make_header(ERL_BIN_REF_SIZE-1,_TAG_HEADER_BIN_REF)

/** @brief The size in words of a BinRef. */
#define ERL_BIN_REF_SIZE (sizeof(BinRef)/sizeof(Eterm))

/** @brief The size in words needed to describe an off-heap binary as a term. */
#define ERL_REFC_BITS_SIZE (ERL_BIN_REF_SIZE+ERL_SUB_BITS_SIZE)

/** @brief A heap bitstring */
typedef struct erl_heap_bits {
    Eterm thing_word;           /* Subtag HEAP_BITS_SUBTAG. */
    Uint size;
    Eterm data[1];              /* The data in the binary. */
} ErlHeapBits;

#define heap_bin_size__(num_bytes)                                            \
  (sizeof(ErlHeapBits)/sizeof(Eterm) - 1 +                                    \
   ((num_bytes) + sizeof(Eterm) - 1)/sizeof(Eterm))

#define heap_bits_size(num_bits)                                              \
    heap_bin_size__(NBYTES(num_bits))

#define header_heap_bits(num_bits) \
  _make_header(heap_bits_size(num_bits)-1,_TAG_HEADER_HEAP_BITS)

/* Maximum number of bytes/bits to place in a heap binary.*/
#define ERL_ONHEAP_BINARY_LIMIT 64
#define ERL_ONHEAP_BITS_LIMIT (ERL_ONHEAP_BINARY_LIMIT * 8)

/** @brief Helper for creating heap bitstrings from arbitrary data */
#define HEAP_BITSTRING(hp, data, offset, size)                                \
    (ASSERT(size <= ERL_ONHEAP_BITS_LIMIT),                                   \
     (hp)[0] = header_heap_bits(size),                                        \
     (hp)[1] = size,                                                          \
     copy_binary_to_buffer((byte*)&(hp)[2], 0, (byte*)data, offset, size),    \
     make_bitstring(hp))

/* ************************************************************************* */
/* Binary construction */

struct erl_bits_state {
    /*
     * Pointer to the beginning of the current binary.
     */
    byte* erts_current_bin_;

    /*
     * Offset in bits into the current binary.
     */
    Uint erts_bin_offset_;
};

/*
 * Reentrant API with the state passed as a parameter.
 * (Except when the current Process* already is a parameter.)
 */
/* the state resides in the current process' scheduler data */
#define ERL_BITS_DECLARE_STATEP struct erl_bits_state *EBS

#define ERL_BITS_RELOAD_STATEP(P)                                              \
    do {                                                                       \
        EBS = &erts_proc_sched_data((P))->registers->aux_regs.d.erl_bits_state;  \
    } while(0)

#define ERL_BITS_DEFINE_STATEP(P) \
    struct erl_bits_state *EBS = \
        &erts_proc_sched_data((P))->registers->aux_regs.d.erl_bits_state

#define ErlBitsState				(*EBS)

#define ERL_BITS_PROTO_0			struct erl_bits_state *EBS
#define ERL_BITS_PROTO_1(PARM1)			struct erl_bits_state *EBS, PARM1
#define ERL_BITS_PROTO_2(PARM1,PARM2)		struct erl_bits_state *EBS, PARM1, PARM2
#define ERL_BITS_PROTO_3(PARM1,PARM2,PARM3)	struct erl_bits_state *EBS, PARM1, PARM2, PARM3
#define ERL_BITS_ARGS_0				EBS
#define ERL_BITS_ARGS_1(ARG1)			EBS, ARG1
#define ERL_BITS_ARGS_2(ARG1,ARG2)		EBS, ARG1, ARG2
#define ERL_BITS_ARGS_3(ARG1,ARG2,ARG3)		EBS, ARG1, ARG2, ARG3

#define erts_bin_offset		(ErlBitsState.erts_bin_offset_)
#define erts_current_bin	(ErlBitsState.erts_current_bin_)

/*
 * Return number of Eterm words needed for allocation with HAlloc(),
 * given a number of bytes.
 */
#define WSIZE(n) ((n + sizeof(Eterm) - 1) / sizeof(Eterm))

/*
 * Define the maximum number of bits in a unit for the binary syntax.
 */
#define ERL_UNIT_BITS 8

/* ************************************************************************* */
/* Helpers for the bitstring syntax */

Eterm erts_bs_start_match_2(Process *p, Eterm Bin, Uint Max);
ErlSubBits *erts_bs_start_match_3(Process *p, Eterm Bin);
Eterm erts_bs_get_integer_2(Process *p, Uint num_bits, unsigned flags, ErlSubBits* sb);
Eterm erts_bs_get_float_2(Process *p, Uint num_bits, unsigned flags, ErlSubBits* sb);

/* These will create heap binaries when appropriate, so they require free space
 * up to BUILD_SUB_BITSTRING_HEAP_NEED. */
Eterm erts_bs_get_binary_2(Process *p, Uint num_bits, unsigned flags, ErlSubBits* sb);
Eterm erts_bs_get_binary_all_2(Process *p, ErlSubBits* sb);

/* Binary construction, new instruction set. */
int erts_new_bs_put_integer(ERL_BITS_PROTO_3(Eterm Integer, Uint num_bits, unsigned flags));
int erts_bs_put_utf8(ERL_BITS_PROTO_1(Eterm Integer));
int erts_bs_put_utf16(ERL_BITS_PROTO_2(Eterm Integer, Uint flags));
int erts_new_bs_put_binary(Process *c_p, Eterm Bin, Uint num_bits);
int erts_new_bs_put_binary_all(Process *c_p, Eterm Bin, Uint unit);
Eterm erts_new_bs_put_float(Process *c_p, Eterm Float, Uint num_bits, int flags);
void erts_new_bs_put_string(ERL_BITS_PROTO_2(byte* iptr, Uint num_bytes));

Uint32 erts_bs_get_unaligned_uint32(ErlSubBits* sb);
Eterm erts_bs_get_utf8(ErlSubBits* sb);
Eterm erts_bs_get_utf16(ErlSubBits* sb, Uint flags);
Eterm erts_bs_append(Process* p, Eterm* reg, Uint live, Eterm build_size_term,
		     Uint extra_words, Uint unit);
Eterm erts_bs_append_checked(Process* p, Eterm* reg, Uint live, Uint size,
                             Uint extra_words, Uint unit);
Eterm erts_bs_private_append(Process* p, Eterm bin, Eterm sz, Uint unit);
Eterm erts_bs_private_append_checked(Process* p, Eterm bin, Uint size, Uint unit);
Eterm erts_bs_init_writable(Process* p, Eterm sz);

/* ************************************************************************* */
/* Copy and comparison routines. */

ERTS_GLB_INLINE void
copy_binary_to_buffer(byte *dst_base, Uint dst_offset,
                      const byte *src_base, Uint src_offset,
                      Uint size);

void erts_copy_bits(const byte* src, size_t soffs, int sdir,
                    byte* dst, size_t doffs, int ddir, size_t n);

ERTS_GLB_INLINE int erts_cmp_bits(const byte* a_ptr,
                                  Uint a_offs,
                                  const byte* b_ptr,
                                  Uint b_offs,
                                  Uint size);
int erts_cmp_bits__(const byte* a_ptr,
                    Uint a_offs,
                    const byte* b_ptr,
                    Uint b_offs,
                    Uint size);

/* ************************************************************************* */
/* Bitstring creation/management */

/** @brief Pins an off-heap binary in place, ensuring that it cannot be moved
 * by the writable-binary optimization. */
void erts_pin_writable_binary(ErlSubBits *sb, BinRef *br);

/* Calculate the heap space for a binary extracted by
 * erts_build_sub_bitstring(). */
ERTS_GLB_INLINE Uint erts_extracted_bitstring_size(Uint size);

/* Conservative estimate of the number of words required for
 * erts_build_sub_bitstring() when the number of bits is unknown. */
#define BUILD_SUB_BITSTRING_HEAP_NEED \
    (MAX(ERL_SUB_BITS_SIZE, heap_bits_size(ERL_ONHEAP_BITS_LIMIT)))

/** @brief Extracts a region from base_bin as a sub-bitstring or heap bitstring,
 * whichever is the most appropriate.
 *
 * Note that you cannot pass sub-bitstrings directly here: to build a
 * sub-bitstring from another, its underlying BinRef* and offset must be
 * extracted and passed here.
 *
 * The caller must ensure that there's enough free space at *hp by using
 * \c erts_extracted_bitstring_size */
Eterm erts_build_sub_bitstring(Eterm **hp,
                               Eterm br_flags,
                               const BinRef *br,
                               const byte *base,
                               Uint offset,
                               Uint size);

/* As erts_build_sub_bitstring, but handles allocation and base_bin
 * extraction. */
Eterm erts_make_sub_bitstring(Process *p, Eterm bitstring, Uint offset, Uint size);
Eterm erts_make_sub_binary(Process *p, Eterm bitstring, Uint offset, Uint size);

Eterm erts_new_bitstring(Process *p, Uint size, byte **datap);
Eterm erts_new_bitstring_refc(Process *p, Uint size, Binary **binp, byte **datap);
Eterm erts_new_bitstring_from_data(Process *p, Uint size, const byte *data);

/* As erts_new_bitstring[_xyz] bit with sizes in bytes rather than bits */
Eterm erts_new_binary(Process *p, Uint size, byte **datap);
Eterm erts_new_binary_refc(Process *p, Uint size, Binary **binp, byte **datap);
Eterm erts_new_binary_from_data(Process *p, Uint size, const byte *data);

Eterm erts_hfact_new_bitstring(ErtsHeapFactory *hfact,
                               Uint extra,
                               Uint size,
                               byte **datap);
Eterm erts_hfact_new_binary_from_data(ErtsHeapFactory *hfact,
                                      Uint extra,
                                      Uint size,
                                      const byte *data);

/** @brief Builds a combined ErlSubBits+BinRef for a full binary, without
 * making a copy if it's smaller than the on-heap bitstring limit.
 *
 * @param hpp must have at least ERL_REFC_BITS_SIZE words available during
 * migration. */
Eterm erts_wrap_refc_bitstring(struct erl_off_heap_header **oh,
                               Uint64 *overhead,
                               Eterm **hpp,
                               Binary *bin,
                               byte *data,
                               Uint offset,
                               Uint size);

#define ERTS_BR_OVERHEAD(oh, br)                                              \
    do {                                                                      \
        (oh)->overhead += ((br)->val)->orig_size / sizeof(Eterm);             \
    } while(0)

/** @brief Extracts a window into the given bitstring.
 *
 * This works on _ALL_ bitstrings, including mutable ones like match contexts
 * or writable binaries (where the returned values reflect the current state).
 *
 * In most routines that only look at the data, this lets us gloss over
 * the fact that these terms are special, making it safe for the compiler to
 * pass them to any routine known not to alias `Bin` itself. */
#define ERTS_GET_BITSTRING(Bin,                                               \
                           Base,                                              \
                           Offset,                                            \
                           Size)                                              \
    do {                                                                      \
        ERTS_DECLARE_DUMMY(const BinRef *_unused_br);                         \
        ERTS_DECLARE_DUMMY(Eterm _unused_br_tag);                             \
        ERTS_GET_BITSTRING_REF(Bin,                                           \
                               _unused_br_tag,                                \
                               _unused_br,                                    \
                               Base,                                          \
                               Offset,                                        \
                               Size);                                         \
    } while (0)

/** @brief As \c ERTS_GET_BITSTRING but also extracts the underlying binary
 * reference, if any. */
#define ERTS_GET_BITSTRING_REF(Bin, RefFlags, Ref, Base, Offset, Size)        \
    do {                                                                      \
        const Eterm *_unboxed = bitstring_val(Bin);                           \
        if (*_unboxed == HEADER_SUB_BITS) {                                   \
            ErlSubBits* _sb = (ErlSubBits*)_unboxed;                          \
            BinRef *_br;                                                      \
            _unboxed = boxed_val(_sb->orig);                                  \
            /* Match contexts may refer to on-heap bitstrings */              \
            _br = (*_unboxed == HEADER_BIN_REF) ? (BinRef*)_unboxed : NULL;   \
            ASSERT(erl_sub_bits_is_match_context(_sb) || _br);                \
            if (ERTS_LIKELY(!erl_sub_bits_was_writable(_sb))) {               \
                Base = (byte*)(_sb->base_flags & ~ERL_SUB_BITS_FLAG_MASK);    \
            } else {                                                          \
                /* If the source was writable its underlying binary may */    \
                /* move at any point, so we need to follow it instead of */   \
                /* using the stored pointer. */                               \
                Base = (byte*)((_br->val)->orig_bytes);                       \
                if (!((_br->val)->intern.flags & BIN_FLAG_WRITABLE)) {        \
                    _sb->base_flags = (UWord)Base;                            \
                }                                                             \
            }                                                                 \
            Offset = _sb->start;                                              \
            Size = _sb->end - _sb->start;                                     \
            RefFlags = _sb->orig & TAG_PTR_MASK__;                            \
            Ref = _br;                                                        \
        } else {                                                              \
            const ErlHeapBits *_hb = ((ErlHeapBits*)_unboxed);                \
            Base = (byte*)&_hb->data[0];                                      \
            Size = _hb->size;                                                 \
            Offset = 0;                                                       \
            RefFlags = 0;                                                     \
            Ref = NULL;                                                       \
        }                                                                     \
    } while (0)

/** @brief As \c ERTS_GET_BITSTRING_REF but also pins writable binaries in
 * place, shrinking and preventing them from being reallocated. */
#define ERTS_PIN_BITSTRING(Bin, RefFlags, Ref, Base, Offset, Size)            \
    do {                                                                      \
        const Eterm *_unboxed = bitstring_val(Bin);                           \
        if (*_unboxed == HEADER_SUB_BITS) {                                   \
            ErlSubBits* _sb = (ErlSubBits*)_unboxed;                          \
            BinRef *_br;                                                      \
            _unboxed = boxed_val(_sb->orig);                                  \
            /* Match contexts may refer to on-heap bitstrings */              \
            _br = (*_unboxed == HEADER_BIN_REF) ? (BinRef*)_unboxed : NULL;   \
            ASSERT(erl_sub_bits_is_match_context(_sb) || _br);                \
            if (ERTS_LIKELY(_br != NULL)) {                                   \
                /* Pinning updates base and flags when necessary. */          \
                erts_pin_writable_binary(_sb, _br);                           \
            }                                                                 \
            Base = (byte*)(_sb->base_flags & ~ERL_SUB_BITS_FLAG_MASK);        \
            Offset = _sb->start;                                              \
            Size = _sb->end - _sb->start;                                     \
            RefFlags = _sb->orig & TAG_PTR_MASK__;                            \
            Ref = _br;                                                        \
        } else {                                                              \
            const ErlHeapBits *_hb = ((ErlHeapBits*)_unboxed);                \
            Base = (byte*)&_hb->data[0];                                      \
            Size = _hb->size;                                                 \
            Offset = 0;                                                       \
            Ref = NULL;                                                       \
        }                                                                     \
    } while (0)

ERTS_GLB_INLINE const byte*
erts_get_aligned_binary_bytes_extra(Eterm bin,
                                    Uint *size_ptr,
                                    const byte **base_ptr,
                                    ErtsAlcType_t allocator,
                                    Uint extra);
ERTS_GLB_INLINE const byte*
erts_get_aligned_binary_bytes(Eterm bin,
                              Uint *size_ptr,
                              const byte** base_ptr);
ERTS_GLB_INLINE void
erts_free_aligned_binary_bytes_extra(const byte* buf, ErtsAlcType_t);
ERTS_GLB_INLINE void
erts_free_aligned_binary_bytes(const byte* buf);

/*
 * Flags for bs_create_bin / bs_get_* / bs_put_* / bs_init* instructions.
 */

#define BSF_ALIGNED 1		/* Field is guaranteed to be byte-aligned. */
#define BSF_LITTLE 2		/* Field is little-endian (otherwise big-endian). */
#define BSF_SIGNED 4		/* Field is signed (otherwise unsigned). */
#define BSF_EXACT 8		/* Size in bs_init is exact. */
#define BSF_NATIVE 16		/* Native endian. */

/*
 * Binary construction operations.
 */

#define BSC_APPEND              0
#define BSC_PRIVATE_APPEND      1
#define BSC_BINARY              2
#define BSC_BINARY_FIXED_SIZE   3
#define BSC_BINARY_ALL          4
#define BSC_FLOAT               5
#define BSC_FLOAT_FIXED_SIZE    6
#define BSC_INTEGER             7
#define BSC_INTEGER_FIXED_SIZE  8
#define BSC_STRING              9
#define BSC_UTF8               10
#define BSC_UTF16              11
#define BSC_UTF32              12

#define BSC_NUM_ARGS            5

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erl_sub_bits_init(ErlSubBits *sb, UWord flags, Eterm orig, const void *base,
                  Uint offset, Uint size)
{
    /* The data pointers that we produce are all aligned, but unaligned ones
     * can sneak in through resource binaries or the likes, in which case we
     * cannot use the lower bits for storage.
     *
     * To handle this corner case, we'll adjust the incoming pointer and shift
     * the start of the bitstring to fit. */
    Uint adjustment = (UWord)base & ERL_SUB_BITS_FLAG_MASK;

    ASSERT(is_boxed(orig));
    ASSERT(!(flags & ~ERL_SUB_BITS_FLAG_MASK));

#ifdef DEBUG
    if (*boxed_val(orig) == HEADER_BIN_REF) {
        Binary *bin = ((BinRef*)boxed_val(orig))->val;
        ASSERT((flags & ERL_SUB_BITS_FLAG_VOLATILE) ||
               !(bin->intern.flags & BIN_FLAG_WRITABLE));
    } else {
        ASSERT(flags == ERL_SUB_BITS_FLAGS_MATCH_CONTEXT);
    }
#endif

    sb->thing_word = HEADER_SUB_BITS;
    sb->start = offset + adjustment * 8;
    sb->end = sb->start + size;
    sb->base_flags = ((UWord)base - adjustment) | flags;
    sb->orig = orig;
}

ERTS_GLB_INLINE void
erl_sub_bits_update_moved(ErlSubBits *sb, Eterm orig)
{
    Eterm *ptr = ptr_val(orig);

    if (thing_subtag(*ptr) == HEAP_BITS_SUBTAG) {
        UWord new_base = (UWord)&((ErlHeapBits*)ptr)->data;

        /* Only match contexts can refer to on-heap bitstrings, ordinary
         * ErlSubBits _must_ point to a BinRef at all times. */
        ASSERT(erl_sub_bits_is_match_context(sb));

        /* Heap alignment guarantees that we don't need to adjust offset
         * according to the base here. */
        ASSERT(!(new_base & ERL_SUB_BITS_FLAG_MASK));
        sb->base_flags = new_base | (sb->base_flags & ERL_SUB_BITS_FLAG_MASK);
    }
}

ERTS_GLB_INLINE void
copy_binary_to_buffer(byte *dst_base, Uint dst_offset,
                      const byte *src_base, Uint src_offset,
                      Uint size)
{
    if (size > 0) {
        dst_base += BYTE_OFFSET(dst_offset);
        src_base += BYTE_OFFSET(src_offset);

        if (((dst_offset | src_offset | size) & 7) == 0) {
            sys_memcpy(dst_base, src_base, BYTE_SIZE(size));
        } else {
            erts_copy_bits(src_base, BIT_OFFSET(src_offset), 1,
                           dst_base, BIT_OFFSET(dst_offset), 1,
                           size);
        }
    }
}

ERTS_GLB_INLINE int
erts_cmp_bits(const byte* a_ptr,
              Uint a_offs,
              const byte* b_ptr,
              Uint b_offs,
              Uint size)
{
    if (size > 0) {
        a_ptr += BYTE_OFFSET(a_offs);
        b_ptr += BYTE_OFFSET(b_offs);

        if (((a_offs | b_offs | size) & 7) == 0) {
            return sys_memcmp(a_ptr, b_ptr, BYTE_SIZE(size));
        }

        return erts_cmp_bits__(a_ptr,
                               BIT_OFFSET(a_offs),
                               b_ptr,
                               BIT_OFFSET(b_offs),
                               size);
    }

    return 0;
}

ERTS_GLB_INLINE Uint
erts_extracted_bitstring_size(Uint size)
{
    if (size <= ERL_ONHEAP_BITS_LIMIT) {
        return heap_bits_size(size);
    } else {
        ERTS_CT_ASSERT(ERL_SUB_BITS_SIZE <= ERL_ONHEAP_BINARY_LIMIT);
        return ERL_SUB_BITS_SIZE;
    }
}

ERTS_GLB_INLINE const byte*
erts_get_aligned_binary_bytes(Eterm bin, Uint *size_ptr, const byte **base_ptr)
{
    return erts_get_aligned_binary_bytes_extra(bin,
                                               size_ptr,
                                               base_ptr,
                                               ERTS_ALC_T_TMP,
                                               0);
}

ERTS_GLB_INLINE const byte*
erts_get_aligned_binary_bytes_extra(Eterm bin,
                                    Uint *size_ptr,
                                    const byte **base_ptr,
                                    ErtsAlcType_t allocator,
                                    Uint extra)
{
    if (is_bitstring(bin)) {
        Uint offset, size;
        const byte *base;

        ERTS_GET_BITSTRING(bin, base, offset, size);

        if (TAIL_BITS(size) == 0) {
            *size_ptr = BYTE_SIZE(size);

            if (BIT_OFFSET(offset) != 0) {
                byte *bytes = (byte*)erts_alloc(allocator,
                                                NBYTES(size) + extra);
                *base_ptr = bytes;

                erts_copy_bits(base, offset, 1, &bytes[extra], 0, 1, size);
                return &bytes[extra];
            }

            return &base[BYTE_OFFSET(offset)];
        }
    }

    return NULL;
}

ERTS_GLB_INLINE void
erts_free_aligned_binary_bytes_extra(const byte *bytes, ErtsAlcType_t allocator)
{
    if (bytes) {
        erts_free(allocator, (void*)bytes);
    }
}

ERTS_GLB_INLINE void
erts_free_aligned_binary_bytes(const byte *bytes)
{
    erts_free_aligned_binary_bytes_extra(bytes, ERTS_ALC_T_TMP);
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* __ERL_BITS_H__ */
