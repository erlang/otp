/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2016. All Rights Reserved.
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

/*
 * This structure represents a binary to be matched.
 */

typedef struct erl_bin_match_buffer {
    Eterm orig;			/* Original binary term. */
    byte* base;			/* Current position in binary. */
    Uint offset;		/* Offset in bits. */
    size_t size;		/* Size of binary in bits. */
} ErlBinMatchBuffer;

struct erl_bits_state {
    /*
     * Used for building binaries.
     */
    byte *byte_buf_;
    int byte_buf_len_;
    /*
     * Used for building binaries using the new instruction set.
     */
    byte* erts_current_bin_;	/* Pointer to beginning of current binary. */
    /*
     * Offset in bits into the current binary (new instruction set) or
     * buffer (old instruction set).
     */
    Uint erts_bin_offset_;
    /*
     * Whether the current binary is writable.
     */
     unsigned erts_writable_bin_;
};

typedef struct erl_bin_match_struct{
  Eterm thing_word;
  ErlBinMatchBuffer mb;		/* Present match buffer */
  Eterm save_offset[1];		/* Saved offsets */
} ErlBinMatchState;

#define ERL_BIN_MATCHSTATE_SIZE(_Max) ((sizeof(ErlBinMatchState) + (_Max)*sizeof(Eterm))/sizeof(Eterm)) 
#define HEADER_BIN_MATCHSTATE(_Max) _make_header(ERL_BIN_MATCHSTATE_SIZE((_Max))-1, _TAG_HEADER_BIN_MATCHSTATE)
#define HEADER_NUM_SLOTS(hdr) (header_arity(hdr)-sizeof(ErlBinMatchState)/sizeof(Eterm)+1)

#define make_matchstate(_Ms) make_boxed((Eterm*)(_Ms))  
#define ms_matchbuffer(_Ms) &(((ErlBinMatchState*) boxed_val(_Ms))->mb)


#if defined(ERTS_SMP)
#define ERL_BITS_REENTRANT
#else
/* uncomment to test the reentrant API in the non-SMP runtime system */
/* #define ERL_BITS_REENTRANT */
#endif

#ifdef ERL_BITS_REENTRANT

/*
 * Reentrant API with the state passed as a parameter.
 * (Except when the current Process* already is a parameter.)
 */
#ifdef ERTS_SMP
/* the state resides in the current process' scheduler data */
#define ERL_BITS_DECLARE_STATEP			struct erl_bits_state *EBS
#define ERL_BITS_RELOAD_STATEP(P)		do{EBS = &erts_proc_sched_data((P))->erl_bits_state;}while(0)
#define ERL_BITS_DEFINE_STATEP(P)		struct erl_bits_state *EBS = &erts_proc_sched_data((P))->erl_bits_state
#else
/* reentrant API but with a hidden single global state, for testing only */
extern struct erl_bits_state ErlBitsState_;
#define ERL_BITS_DECLARE_STATEP			struct erl_bits_state *EBS = &ErlBitsState_
#define ERL_BITS_RELOAD_STATEP(P)		do{}while(0)
#define ERL_BITS_DEFINE_STATEP(P)		ERL_BITS_DECLARE_STATEP
#endif
#define ErlBitsState				(*EBS)

#define ERL_BITS_PROTO_0			struct erl_bits_state *EBS
#define ERL_BITS_PROTO_1(PARM1)			struct erl_bits_state *EBS, PARM1
#define ERL_BITS_PROTO_2(PARM1,PARM2)		struct erl_bits_state *EBS, PARM1, PARM2
#define ERL_BITS_PROTO_3(PARM1,PARM2,PARM3)	struct erl_bits_state *EBS, PARM1, PARM2, PARM3
#define ERL_BITS_ARGS_0				EBS
#define ERL_BITS_ARGS_1(ARG1)			EBS, ARG1
#define ERL_BITS_ARGS_2(ARG1,ARG2)		EBS, ARG1, ARG2
#define ERL_BITS_ARGS_3(ARG1,ARG2,ARG3)		EBS, ARG1, ARG2, ARG3

#else	/* ERL_BITS_REENTRANT */

/*
 * Non-reentrant API with a single global state.
 */
extern struct erl_bits_state ErlBitsState;
#define ERL_BITS_DECLARE_STATEP			/*empty*/
#define ERL_BITS_RELOAD_STATEP(P)		do{}while(0)
#define ERL_BITS_DEFINE_STATEP(P)		/*empty*/

#define ERL_BITS_PROTO_0			void
#define ERL_BITS_PROTO_1(PARM1)			PARM1
#define ERL_BITS_PROTO_2(PARM1,PARM2)		PARM1, PARM2
#define ERL_BITS_PROTO_3(PARM1,PARM2,PARM3)	PARM1, PARM2, PARM3
#define ERL_BITS_ARGS_0				/*empty*/
#define ERL_BITS_ARGS_1(ARG1)			ARG1
#define ERL_BITS_ARGS_2(ARG1,ARG2)		ARG1, ARG2
#define ERL_BITS_ARGS_3(ARG1,ARG2,ARG3)		ARG1, ARG2, ARG3

#endif	/* ERL_BITS_REENTRANT */

#define erts_bin_offset		(ErlBitsState.erts_bin_offset_)
#define erts_current_bin	(ErlBitsState.erts_current_bin_)
#define erts_writable_bin       (ErlBitsState.erts_writable_bin_)

#define copy_binary_to_buffer(DstBuffer, DstBufOffset, SrcBuffer, SrcBufferOffset, NumBits) \
  do {											    \
    if (BIT_OFFSET(DstBufOffset) == 0 && (SrcBufferOffset == 0) &&			    \
        (BIT_OFFSET(NumBits)==0)) {							    \
      sys_memcpy(DstBuffer+BYTE_OFFSET(DstBufOffset),					    \
		 SrcBuffer, NBYTES(NumBits));						    \
    } else {										    \
      erts_copy_bits(SrcBuffer, SrcBufferOffset, 1,					    \
        (byte*)DstBuffer, DstBufOffset, 1, NumBits);					    \
    }											    \
  }  while (0)

void erts_init_bits(void);	/* Initialization once. */
#ifdef ERTS_SMP
void erts_bits_init_state(ERL_BITS_PROTO_0);
void erts_bits_destroy_state(ERL_BITS_PROTO_0);
#endif


/*
 * NBYTES(x) returns the number of bytes needed to store x bits.
 */

#define NBYTES(x)  (((Uint64)(x) + (Uint64) 7) >> 3) 
#define BYTE_OFFSET(ofs) ((Uint) (ofs) >> 3)
#define BIT_OFFSET(ofs) ((ofs) & 7)

/*
 * Return number of Eterm words needed for allocation with HAlloc(),
 * given a number of bytes.
 */
#define WSIZE(n) ((n + sizeof(Eterm) - 1) / sizeof(Eterm))

/*
 * Binary matching.
 */

Eterm erts_bs_start_match_2(Process *p, Eterm Bin, Uint Max);
Eterm erts_bs_get_integer_2(Process *p, Uint num_bits, unsigned flags, ErlBinMatchBuffer* mb);
Eterm erts_bs_get_binary_2(Process *p, Uint num_bits, unsigned flags, ErlBinMatchBuffer* mb);
Eterm erts_bs_get_float_2(Process *p, Uint num_bits, unsigned flags, ErlBinMatchBuffer* mb);
Eterm erts_bs_get_binary_all_2(Process *p, ErlBinMatchBuffer* mb);

/*
 * Binary construction, new instruction set.
 */

int erts_new_bs_put_integer(ERL_BITS_PROTO_3(Eterm Integer, Uint num_bits, unsigned flags));
int erts_bs_put_utf8(ERL_BITS_PROTO_1(Eterm Integer));
int erts_bs_put_utf16(ERL_BITS_PROTO_2(Eterm Integer, Uint flags));
int erts_new_bs_put_binary(ERL_BITS_PROTO_2(Eterm Bin, Uint num_bits));
int erts_new_bs_put_binary_all(ERL_BITS_PROTO_2(Eterm Bin, Uint unit));
int erts_new_bs_put_float(Process *c_p, Eterm Float, Uint num_bits, int flags);
void erts_new_bs_put_string(ERL_BITS_PROTO_2(byte* iptr, Uint num_bytes));

Uint erts_bits_bufs_size(void);
Uint32 erts_bs_get_unaligned_uint32(ErlBinMatchBuffer* mb);
void erts_align_utf8_bytes(ErlBinMatchBuffer* mb, byte* buf);
Eterm erts_bs_get_utf8(ErlBinMatchBuffer* mb);
Eterm erts_bs_get_utf16(ErlBinMatchBuffer* mb, Uint flags);
Eterm erts_bs_append(Process* p, Eterm* reg, Uint live, Eterm build_size_term,
		     Uint extra_words, Uint unit);
Eterm erts_bs_private_append(Process* p, Eterm bin, Eterm sz, Uint unit);
Eterm erts_bs_init_writable(Process* p, Eterm sz);

/*
 * Common utilities.
 */
void erts_copy_bits(byte* src, size_t soffs, int sdir,
		    byte* dst, size_t doffs,int ddir, size_t n);        
int erts_cmp_bits(byte* a_ptr, size_t a_offs, byte* b_ptr, size_t b_offs, size_t size); 

/*
 * Flags for bs_get_* / bs_put_* / bs_init* instructions.
 */

#define BSF_ALIGNED 1		/* Field is guaranteed to be byte-aligned. */
#define BSF_LITTLE 2		/* Field is little-endian (otherwise big-endian). */
#define BSF_SIGNED 4		/* Field is signed (otherwise unsigned). */
#define BSF_EXACT 8		/* Size in bs_init is exact. */
#define BSF_NATIVE 16		/* Native endian. */

#endif /* __ERL_BITS_H__ */
