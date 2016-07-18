/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
#include "config.h"
#endif
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "erl_bits.h"
#include "erl_message.h"
/* this sucks, but the compiler needs data for all platforms */
#include "hipe_arm_asm.h"
#undef P
#undef NSP
#undef HP
#undef TEMP_LR
#undef SAVE_CACHED_STATE
#undef RESTORE_CACHED_STATE
#undef SAVE_CONTEXT_QUICK
#undef RESTORE_CONTEXT_QUICK
#undef SAVE_CONTEXT_BIF
#undef RESTORE_CONTEXT_BIF
#undef SAVE_CONTEXT_GC
#undef RESTORE_CONTEXT_GC
#undef NR_ARG_REGS
#undef LOAD_ARG_REGS
#undef STORE_ARG_REGS
#undef TEMP_ARG0
#undef TEMP_ARG1
#undef TEMP_ARG2
#undef ARG0
#undef ARG1
#undef ARG2
#undef ARG3
#undef ARG4
#undef ARG5
#include "hipe_ppc_asm.h"
#undef P
#undef NSP
#undef HP
#undef TEMP_LR
#undef SAVE_CACHED_STATE
#undef RESTORE_CACHED_STATE
#undef SAVE_CONTEXT_QUICK
#undef RESTORE_CONTEXT_QUICK
#undef SAVE_CONTEXT_BIF
#undef RESTORE_CONTEXT_BIF
#undef SAVE_CONTEXT_GC
#undef RESTORE_CONTEXT_GC
#undef NR_ARG_REGS
#undef LOAD_ARG_REGS
#undef STORE_ARG_REGS
#undef TEMP_ARG0
#undef TEMP_ARG1
#undef TEMP_ARG2
#undef ARG0
#undef ARG1
#undef ARG2
#undef ARG3
#undef ARG4
#undef ARG5
#include "hipe_amd64_asm.h"
#undef P
#undef HP
#undef NSP
#undef TEMP_ARG0
#undef TEMP_ARG1
#undef TEMP_ARG2
#undef ARG0
#undef ARG1
#undef ARG2
#undef ARG3
#undef ARG4
#undef ARG5
#undef SAVE_HP
#undef RESTORE_HP
#undef SAVE_CSP
#undef RESTORE_CSP
#undef SAVE_CACHED_STATE
#undef RESTORE_CACHED_STATE
#undef SWITCH_C_TO_ERLANG_QUICK
#undef SWITCH_ERLANG_TO_C_QUICK
#undef SWITCH_C_TO_ERLANG
#undef SWITCH_ERLANG_TO_C
#undef NR_ARG_REGS
#undef LEAF_WORDS
#undef TEMP_RV
#undef LOAD_ARG_REGS
#undef STORE_ARG_REGS
#undef NSP_CALL
#undef NSP_RETN
#undef NSP_RET0
#include "hipe_x86_asm.h"
#undef P
#undef HP
#undef NSP
#undef TEMP0
#undef TEMP1
#undef ARG0
#undef ARG1
#undef ARG2
#undef SAVE_HP
#undef RESTORE_HP
#undef SAVE_CSP
#undef RESTORE_CSP
#undef SAVE_CACHED_STATE
#undef RESTORE_CACHED_STATE
#undef SWITCH_C_TO_ERLANG_QUICK
#undef SWITCH_ERLANG_TO_C_QUICK
#undef NR_ARG_REGS
#undef LEAF_WORDS
#undef TEMP_RV
#undef LOAD_ARG_REGS
#undef STORE_ARG_REGS
#include "hipe_sparc_asm.h"
#include "erl_binary.h"

#define ARRAY_SIZE(x)	(sizeof(x) / sizeof((x)[0]))

#define field_sizeof(STRUCT, FIELD) (sizeof(((STRUCT *)0)->FIELD))

static const unsigned int CRCTABLE[256] = {
    0x00000000, 0x77073096, 0xEE0E612C, 0x990951BA,
    0x076DC419, 0x706AF48F, 0xE963A535, 0x9E6495A3,
    0x0EDB8832, 0x79DCB8A4, 0xE0D5E91E, 0x97D2D988,
    0x09B64C2B, 0x7EB17CBD, 0xE7B82D07, 0x90BF1D91,
    0x1DB71064, 0x6AB020F2, 0xF3B97148, 0x84BE41DE,
    0x1ADAD47D, 0x6DDDE4EB, 0xF4D4B551, 0x83D385C7,
    0x136C9856, 0x646BA8C0, 0xFD62F97A, 0x8A65C9EC,
    0x14015C4F, 0x63066CD9, 0xFA0F3D63, 0x8D080DF5,
    0x3B6E20C8, 0x4C69105E, 0xD56041E4, 0xA2677172,
    0x3C03E4D1, 0x4B04D447, 0xD20D85FD, 0xA50AB56B,
    0x35B5A8FA, 0x42B2986C, 0xDBBBC9D6, 0xACBCF940,
    0x32D86CE3, 0x45DF5C75, 0xDCD60DCF, 0xABD13D59,
    0x26D930AC, 0x51DE003A, 0xC8D75180, 0xBFD06116,
    0x21B4F4B5, 0x56B3C423, 0xCFBA9599, 0xB8BDA50F,
    0x2802B89E, 0x5F058808, 0xC60CD9B2, 0xB10BE924,
    0x2F6F7C87, 0x58684C11, 0xC1611DAB, 0xB6662D3D,
    0x76DC4190, 0x01DB7106, 0x98D220BC, 0xEFD5102A,
    0x71B18589, 0x06B6B51F, 0x9FBFE4A5, 0xE8B8D433,
    0x7807C9A2, 0x0F00F934, 0x9609A88E, 0xE10E9818,
    0x7F6A0DBB, 0x086D3D2D, 0x91646C97, 0xE6635C01,
    0x6B6B51F4, 0x1C6C6162, 0x856530D8, 0xF262004E,
    0x6C0695ED, 0x1B01A57B, 0x8208F4C1, 0xF50FC457,
    0x65B0D9C6, 0x12B7E950, 0x8BBEB8EA, 0xFCB9887C,
    0x62DD1DDF, 0x15DA2D49, 0x8CD37CF3, 0xFBD44C65,
    0x4DB26158, 0x3AB551CE, 0xA3BC0074, 0xD4BB30E2,
    0x4ADFA541, 0x3DD895D7, 0xA4D1C46D, 0xD3D6F4FB,
    0x4369E96A, 0x346ED9FC, 0xAD678846, 0xDA60B8D0,
    0x44042D73, 0x33031DE5, 0xAA0A4C5F, 0xDD0D7CC9,
    0x5005713C, 0x270241AA, 0xBE0B1010, 0xC90C2086,
    0x5768B525, 0x206F85B3, 0xB966D409, 0xCE61E49F,
    0x5EDEF90E, 0x29D9C998, 0xB0D09822, 0xC7D7A8B4,
    0x59B33D17, 0x2EB40D81, 0xB7BD5C3B, 0xC0BA6CAD,
    0xEDB88320, 0x9ABFB3B6, 0x03B6E20C, 0x74B1D29A,
    0xEAD54739, 0x9DD277AF, 0x04DB2615, 0x73DC1683,
    0xE3630B12, 0x94643B84, 0x0D6D6A3E, 0x7A6A5AA8,
    0xE40ECF0B, 0x9309FF9D, 0x0A00AE27, 0x7D079EB1,
    0xF00F9344, 0x8708A3D2, 0x1E01F268, 0x6906C2FE,
    0xF762575D, 0x806567CB, 0x196C3671, 0x6E6B06E7,
    0xFED41B76, 0x89D32BE0, 0x10DA7A5A, 0x67DD4ACC,
    0xF9B9DF6F, 0x8EBEEFF9, 0x17B7BE43, 0x60B08ED5,
    0xD6D6A3E8, 0xA1D1937E, 0x38D8C2C4, 0x4FDFF252,
    0xD1BB67F1, 0xA6BC5767, 0x3FB506DD, 0x48B2364B,
    0xD80D2BDA, 0xAF0A1B4C, 0x36034AF6, 0x41047A60,
    0xDF60EFC3, 0xA867DF55, 0x316E8EEF, 0x4669BE79,
    0xCB61B38C, 0xBC66831A, 0x256FD2A0, 0x5268E236,
    0xCC0C7795, 0xBB0B4703, 0x220216B9, 0x5505262F,
    0xC5BA3BBE, 0xB2BD0B28, 0x2BB45A92, 0x5CB36A04,
    0xC2D7FFA7, 0xB5D0CF31, 0x2CD99E8B, 0x5BDEAE1D,
    0x9B64C2B0, 0xEC63F226, 0x756AA39C, 0x026D930A,
    0x9C0906A9, 0xEB0E363F, 0x72076785, 0x05005713,
    0x95BF4A82, 0xE2B87A14, 0x7BB12BAE, 0x0CB61B38,
    0x92D28E9B, 0xE5D5BE0D, 0x7CDCEFB7, 0x0BDBDF21,
    0x86D3D2D4, 0xF1D4E242, 0x68DDB3F8, 0x1FDA836E,
    0x81BE16CD, 0xF6B9265B, 0x6FB077E1, 0x18B74777,
    0x88085AE6, 0xFF0F6A70, 0x66063BCA, 0x11010B5C,
    0x8F659EFF, 0xF862AE69, 0x616BFFD3, 0x166CCF45,
    0xA00AE278, 0xD70DD2EE, 0x4E048354, 0x3903B3C2,
    0xA7672661, 0xD06016F7, 0x4969474D, 0x3E6E77DB,
    0xAED16A4A, 0xD9D65ADC, 0x40DF0B66, 0x37D83BF0,
    0xA9BCAE53, 0xDEBB9EC5, 0x47B2CF7F, 0x30B5FFE9,
    0xBDBDF21C, 0xCABAC28A, 0x53B39330, 0x24B4A3A6,
    0xBAD03605, 0xCDD70693, 0x54DE5729, 0x23D967BF,
    0xB3667A2E, 0xC4614AB8, 0x5D681B02, 0x2A6F2B94,
    0xB40BBE37, 0xC30C8EA1, 0x5A05DF1B, 0x2D02EF8D,
};

/* For hipe cross compiler. Hard code all values.
   No calls by hipe compiler to query the running emulator.
*/
static int is_xcomp = 0;

/*
 *  The algorithm for calculating the 32 bit CRC checksum is based upon
 *  documentation and algorithms provided by Dr. Ross N. Williams in the
 *  document "A Painless Guide to CRC Error Detection Algorithms."
 *  This document may be downloaded from
 *  ftp://ftp.rocksoft.com/cliens/rocksoft/papers/crc_v3.txt
 *  as of 12/15/1998. Dr. Williams has placed this document and algorithms
 *  in the public domain.
 */
static unsigned int crc_init(void)
{
    return 0xFFFFFFFF;
}

static unsigned int
crc_update_buf(unsigned int crc_value,
	       const void *buf,
	       unsigned int length)
{
    const unsigned char *tab;

    tab = (const unsigned char*)buf;
    for (; length > 0; --length) {
	unsigned char t = (crc_value >> 24) & 0xFF;
	crc_value = (crc_value << 8) | *tab++;
	crc_value ^= CRCTABLE[t];
    }
    return crc_value;
}

static unsigned int
crc_update_int(unsigned int crc_value, const int *p)
{
    return crc_update_buf(crc_value, p, sizeof *p);
}

/*
 * Runtime system parameters.
 * Invariant for a given CPU architecture.
 * (Would be invariant for 32 bit CPUs if SPARC didn't
 * enlarge the def_arg_reg[] array.)
 */
static const struct literal {
    const char *name;
    int value;
} literals[] = {
    /* process flags bits */
    {  "F_TIMO", F_TIMO },

    /* freason codes */
    { "FREASON_TRAP", TRAP },

    /* funs */
#ifdef HIPE
    { "EFE_NATIVE_ADDRESS", offsetof(struct erl_fun_entry, native_address) },
#endif
    { "EFE_REFC", offsetof(struct erl_fun_entry, refc) },
    { "EFT_THING", offsetof(struct erl_fun_thing, thing_word) },

    /* bit syntax */
    { "BSF_ALIGNED", BSF_ALIGNED},
    { "PB_ACTIVE_WRITER", PB_ACTIVE_WRITER},
    { "PB_IS_WRITABLE", PB_IS_WRITABLE},
    { "MB_ORIG", offsetof(struct erl_bin_match_buffer, orig) },
    { "MB_BASE", offsetof(struct erl_bin_match_buffer, base) },
    { "MB_OFFSET", offsetof(struct erl_bin_match_buffer, offset) },
    { "MB_SIZE", offsetof(struct erl_bin_match_buffer, size) },
    { "PROC_BIN_THING_WORD", offsetof(struct proc_bin, thing_word) },
    { "PROC_BIN_BINSIZE", offsetof(struct proc_bin, size) },
    { "PROC_BIN_NEXT", offsetof(struct proc_bin, next) },
    { "PROC_BIN_VAL", offsetof(struct proc_bin, val) },
    { "PROC_BIN_BYTES", offsetof(struct proc_bin, bytes) },
    { "PROC_BIN_FLAGS", offsetof(struct proc_bin, flags) },
    { "PROC_BIN_WORDSIZE", PROC_BIN_SIZE},
    { "SUB_BIN_THING_WORD", offsetof(struct erl_sub_bin, thing_word) },
    { "SUB_BIN_BINSIZE", offsetof(struct erl_sub_bin, size) },
    { "SUB_BIN_BITSIZE", offsetof(struct erl_sub_bin, bitsize) },
    { "SUB_BIN_OFFS", offsetof(struct erl_sub_bin, offs) },
    { "SUB_BIN_BITOFFS", offsetof(struct erl_sub_bin, bitoffs) },
    { "SUB_BIN_WRITABLE", offsetof(struct erl_sub_bin, is_writable) },
    { "SUB_BIN_ORIG", offsetof(struct erl_sub_bin, orig) },
    { "SUB_BIN_WORDSIZE", ERL_SUB_BIN_SIZE},
    { "HEAP_BIN_THING_WORD", offsetof(struct erl_heap_bin, thing_word) },
    { "HEAP_BIN_SIZE", offsetof(struct erl_heap_bin, size) },
    { "HEAP_BIN_DATA", offsetof(struct erl_heap_bin, data) },
    { "BINARY_ORIG_SIZE", offsetof(struct binary, orig_size) },
    { "BINARY_ORIG_BYTES", offsetof(struct binary, orig_bytes) },
    { "MAX_HEAP_BIN_SIZE", ERL_ONHEAP_BIN_LIMIT},
    { "MS_THING_WORD", offsetof(struct erl_bin_match_struct, thing_word)},
    { "MS_MATCHBUFFER", offsetof(struct erl_bin_match_struct, mb)},
    { "MS_SAVEOFFSET", offsetof(struct erl_bin_match_struct, save_offset)},

    { "MS_MIN_SIZE", ERL_BIN_MATCHSTATE_SIZE(0)},

    { "MB_ORIG_SIZE", field_sizeof(struct erl_bin_match_buffer, orig) },
    { "MB_BASE_SIZE", field_sizeof(struct erl_bin_match_buffer, base) },
    { "MB_OFFSET_SIZE", field_sizeof(struct erl_bin_match_buffer, offset) },
    { "MB_SIZE_SIZE", field_sizeof(struct erl_bin_match_buffer, size) },
    { "PROC_BIN_THING_WORD_SIZE", field_sizeof(struct proc_bin, thing_word) },
    { "PROC_BIN_BINSIZE_SIZE", field_sizeof(struct proc_bin, size) },
    { "PROC_BIN_NEXT_SIZE", field_sizeof(struct proc_bin, next) },
    { "PROC_BIN_VAL_SIZE", field_sizeof(struct proc_bin, val) },
    { "PROC_BIN_BYTES_SIZE", field_sizeof(struct proc_bin, bytes) },
    { "PROC_BIN_FLAGS_SIZE", field_sizeof(struct proc_bin, flags) },
    { "SUB_BIN_THING_WORD_SIZE", field_sizeof(struct erl_sub_bin, thing_word) },
    { "SUB_BIN_BINSIZE_SIZE", field_sizeof(struct erl_sub_bin, size) },
    { "SUB_BIN_BITSIZE_SIZE", field_sizeof(struct erl_sub_bin, bitsize) },
    { "SUB_BIN_OFFS_SIZE", field_sizeof(struct erl_sub_bin, offs) },
    { "SUB_BIN_BITOFFS_SIZE", field_sizeof(struct erl_sub_bin, bitoffs) },
    { "SUB_BIN_WRITABLE_SIZE", field_sizeof(struct erl_sub_bin, is_writable) },
    { "SUB_BIN_ORIG_SIZE", field_sizeof(struct erl_sub_bin, orig) },
    { "HEAP_BIN_THING_WORD_SIZE", field_sizeof(struct erl_heap_bin, thing_word) },
    { "HEAP_BIN_SIZE_SIZE", field_sizeof(struct erl_heap_bin, size) },
    { "HEAP_BIN_DATA_SIZE", field_sizeof(struct erl_heap_bin, data) },
    { "BINARY_ORIG_SIZE_SIZE", field_sizeof(struct binary, orig_size) },
    { "BINARY_ORIG_BYTES_SIZE", field_sizeof(struct binary, orig_bytes) },
    { "MS_THING_WORD_SIZE", field_sizeof(struct erl_bin_match_struct, thing_word)},
    { "MS_SAVEOFFSET_SIZE", field_sizeof(struct erl_bin_match_struct, save_offset)},

    /* messages */
    { "MSG_NEXT", offsetof(struct erl_mesg, next) },

    /* ARM */
    { "ARM_LEAF_WORDS", ARM_LEAF_WORDS },
    { "ARM_NR_ARG_REGS", ARM_NR_ARG_REGS },
    { "ARM_IS_BIG_ENDIAN",
#if defined(__arm__) && defined(__ARMEB__)
      1
#else
      0
#endif
    },

    /* PowerPC */
    { "PPC_LEAF_WORDS", PPC_LEAF_WORDS },
    { "PPC_NR_ARG_REGS", PPC_NR_ARG_REGS },

    /* Amd64 */
    { "AMD64_LEAF_WORDS", AMD64_LEAF_WORDS },
    { "AMD64_NR_ARG_REGS", AMD64_NR_ARG_REGS },
#if AMD64_HP_IN_REGISTER
    { "AMD64_HP_IN_REGISTER", 1 },
    { "AMD64_HEAP_POINTER", AMD64_HEAP_POINTER },
#endif
#if AMD64_FCALLS_IN_REGISTER
    { "AMD64_FCALLS_IN_REGISTER", 1 },
    { "AMD64_FCALLS_REGISTER", AMD64_FCALLS_REGISTER },
#endif
#if AMD64_HEAP_LIMIT_IN_REGISTER
    { "AMD64_HEAP_LIMIT_IN_REGISTER", 1 },
    { "AMD64_HEAP_LIMIT_REGISTER", AMD64_HEAP_LIMIT_REGISTER },
#endif
#if AMD64_SIMULATE_NSP
    { "AMD64_SIMULATE_NSP", 1 },
#endif

    /* x86 */
    { "X86_LEAF_WORDS", X86_LEAF_WORDS },
    { "X86_NR_ARG_REGS", X86_NR_ARG_REGS },
    /* Jag vet att detta suger.. temp dock. */
    { "X86_NR_RET_REGS", 3},
#if X86_HP_IN_ESI
    { "X86_HP_IN_ESI", 1 },
#endif
#if X86_SIMULATE_NSP
    { "X86_SIMULATE_NSP", 1 },
#endif

    /* SPARC */
    { "SPARC_LEAF_WORDS", SPARC_LEAF_WORDS },
    { "SPARC_NR_ARG_REGS", SPARC_NR_ARG_REGS},
};

#define NR_LITERALS	ARRAY_SIZE(literals)

/*
 * Runtime system parameters that generate Erlang atoms.
 */
static const struct atom_literal {
   const char *name;
   const char *value;
} atom_literals[] = {
   { "ARM_ENDIANESS",
#if defined(__arm__) && defined(__ARMEB__)
     "big"
#else
     "little"
#endif
   },
};

#define NR_ATOM_LITERALS ARRAY_SIZE(atom_literals)

/*
 * Runtime system parameters.
 * These depend on configuration options such as heap architecture.
 * The compiler accesses these through hipe_bifs:get_rts_param/1.
 */
struct rts_param {
    unsigned int nr;
    const char *name;
    unsigned int is_defined;
    int value;
};

static const struct rts_param rts_params[] = {
    { 1, "P_OFF_HEAP_FUNS",
      1, offsetof(struct process, off_heap.first)
    },

    { 4, "EFT_NEXT",
      1, offsetof(struct erl_fun_thing, next)
    },

    /* These are always defined, but their values depend on the
       presence or absence of struct erl_fun_thing's "next" field. */
    { 5, "EFT_CREATOR", 1, offsetof(struct erl_fun_thing, creator) },
    { 6, "EFT_FE", 1, offsetof(struct erl_fun_thing, fe) },
#ifdef HIPE
    { 7, "EFT_NATIVE_ADDRESS", 1, offsetof(struct erl_fun_thing, native_address) },
#endif
    { 8, "EFT_ARITY", 1, offsetof(struct erl_fun_thing, arity) },
    { 9, "EFT_NUM_FREE", 1, offsetof(struct erl_fun_thing, num_free) },
    { 10, "EFT_ENV", 1, offsetof(struct erl_fun_thing, env[0]) },
    { 11, "ERL_FUN_SIZE", 1, ERL_FUN_SIZE },

    { 12, "P_SCHED_DATA",
#ifdef ERTS_SMP
      1, offsetof(struct process, scheduler_data)
#endif
    },
    { 14, "P_FP_EXCEPTION",
#if !defined(NO_FPE_SIGNALS) || defined(HIPE)
      1, offsetof(struct process, fp_exception)
#endif
    },
    /* This flag is always defined, but its value is configuration-dependent. */
    { 15, "ERTS_IS_SMP",
      1,
#if defined(ERTS_SMP)
      1
#else
      0
#endif
    },
    /* This flag is always defined, but its value is configuration-dependent. */
    { 16, "ERTS_NO_FPE_SIGNALS",
      1,
#if defined(NO_FPE_SIGNALS)
      1
#else
      0
#endif
    },
    /* This parameter is always defined, but its value depends on ERTS_SMP. */
    { 19, "MSG_MESSAGE",
      1, offsetof(struct erl_mesg, m[0])
    },

    /* Field offsets in a process struct */
    { 22, "P_HP", 1, offsetof(struct process, htop) },
    { 23, "P_HP_LIMIT", 1, offsetof(struct process, stop) },
    { 24, "P_OFF_HEAP_FIRST", 1, offsetof(struct process, off_heap.first) },
    { 25, "P_MBUF", 1, offsetof(struct process, mbuf) },
    { 26, "P_ID", 1, offsetof(struct process, common.id) },
    { 27, "P_FLAGS", 1, offsetof(struct process, flags) },
    { 28, "P_FVALUE", 1, offsetof(struct process, fvalue) },
    { 29, "P_FREASON", 1, offsetof(struct process, freason) },
    { 30, "P_FTRACE", 1, offsetof(struct process, ftrace) },
    { 31, "P_FCALLS", 1, offsetof(struct process, fcalls) },
    { 32, "P_BEAM_IP", 1, offsetof(struct process, i) },
    { 33, "P_ARITY", 1, offsetof(struct process, arity) },
    { 34, "P_ARG0", 1, offsetof(struct process, def_arg_reg[0]) },
    { 35, "P_ARG1", 1, offsetof(struct process, def_arg_reg[1]) },
    { 36, "P_ARG2", 1, offsetof(struct process, def_arg_reg[2]) },
    { 37, "P_ARG3", 1, offsetof(struct process, def_arg_reg[3]) },
    { 38, "P_ARG4", 1, offsetof(struct process, def_arg_reg[4]) },
    { 39, "P_ARG5", 1, offsetof(struct process, def_arg_reg[5]) },
    { 40, "P_NSP", 1, offsetof(struct process, hipe.nsp) },
    { 41, "P_NCALLEE", 1, offsetof(struct process, hipe.u.ncallee) },
    { 42, "P_CLOSURE", 1, offsetof(struct process, hipe.u.closure) },
    { 43, "P_NSP_LIMIT", 1, offsetof(struct process, hipe.nstack) },
    { 44, "P_CSP",
#if defined(__i386__) || defined(__x86_64__)
	1, offsetof(struct process, hipe.ncsp)
#endif
    },
    { 45, "P_NRA",
#if defined(__sparc__) || defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__) || defined(__arm__)
	1, offsetof(struct process, hipe.nra)
#endif
    },
    { 46, "P_NARITY", 1, offsetof(struct process, hipe.narity) },
    { 47, "P_FLOAT_RESULT",
#ifdef NO_FPE_SIGNALS
	1, offsetof(struct process, hipe.float_result)
#endif
    },
    { 48, "P_BIF_CALLEE",
#if defined(ERTS_ENABLE_LOCK_CHECK) && defined(ERTS_SMP)
	1, offsetof(struct process, hipe.bif_callee)
#endif
    },
    { 49, "P_MSG_FIRST", 1, offsetof(struct process, msg.first) },
    { 50, "P_MSG_SAVE", 1, offsetof(struct process, msg.save) },
    { 51, "P_CALLEE_EXP", 1, offsetof(struct process, hipe.u.callee_exp) },

    { 52, "THE_NON_VALUE", 1, (int)THE_NON_VALUE },

    { 53, "P_GCUNSAFE",
#ifdef DEBUG
      1, offsetof(struct process, hipe.gc_is_unsafe)
#endif
    },
};

#define NR_PARAMS	ARRAY_SIZE(rts_params)

static unsigned int literals_crc;
static unsigned int system_crc;

static void compute_crc(void)
{
    unsigned int crc_value;
    unsigned int i;

    crc_value = crc_init();
    for (i = 0; i < NR_LITERALS; ++i)
	crc_value = crc_update_int(crc_value, &literals[i].value);
    crc_value &= 0x07FFFFFF;
    literals_crc = crc_value;

    crc_value = crc_init();
    for (i = 0; i < NR_PARAMS; ++i)
	if (rts_params[i].is_defined)
	    crc_value = crc_update_int(crc_value, &rts_params[i].value);
    crc_value &= 0x07FFFFFF;
    system_crc = crc_value;
}

static void c_define_literal(FILE *fp, const struct literal *literal)
{
    fprintf(fp, "#define %s %d\n", literal->name, literal->value);
}

static void e_define_literal(FILE *fp, const struct literal *literal)
{
    fprintf(fp, "-define(%s, %d).\n", literal->name, literal->value);
}

static void print_literals(FILE *fp, void (*print_literal)(FILE*, const struct literal*))
{
    unsigned int i;

    for (i = 0; i < NR_LITERALS; ++i)
	(*print_literal)(fp, &literals[i]);
}

static void e_define_atom_literal(FILE *fp, const struct atom_literal *atom_literal)
{
    fprintf(fp, "-define(%s, %s).\n", atom_literal->name, atom_literal->value);
}

static void print_atom_literals(FILE *fp, void (*print_atom_literal)(FILE*, const struct atom_literal*))
{
    unsigned int i;

    for (i = 0; i < NR_ATOM_LITERALS; ++i)
	(*print_atom_literal)(fp, &atom_literals[i]);
}

static void c_define_param(FILE *fp, const struct rts_param *param)
{
    if (param->is_defined)
	fprintf(fp, "#define %s %d\n", param->name, param->value);
}

static void c_case_param(FILE *fp, const struct rts_param *param)
{
    fprintf(fp, " \\\n");
    fprintf(fp, "\tcase %u: ", param->nr);
    if (param->is_defined)
	fprintf(fp, "value = %d", param->value);
    else
	fprintf(fp, "is_defined = 0");
    fprintf(fp, "; break;");
}

static void e_define_param(FILE *fp, const struct rts_param *param)
{
    if (is_xcomp) {
	if (param->is_defined)
	    fprintf(fp, "-define(%s, %d).\n", param->name, param->value);
	else
	    fprintf(fp, "-define(%s, []).\n", param->name);	
    }
    else {
	fprintf(fp, "-define(%s, hipe_bifs:get_rts_param(%u)).\n", param->name, param->nr);
    }    
}

static void print_params(FILE *fp, void (*print_param)(FILE*,const struct rts_param*))
{
    unsigned int i;

    for (i = 0; i < NR_PARAMS; ++i)
	(*print_param)(fp, &rts_params[i]);
}

static int do_c(FILE *fp, const char* this_exe)
{
    fprintf(fp, "/* File: hipe_literals.h, generated by %s */\n", this_exe);
    fprintf(fp, "#ifndef __HIPE_LITERALS_H__\n");
    fprintf(fp, "#define __HIPE_LITERALS_H__\n\n");
    print_literals(fp, c_define_literal);
    print_params(fp, c_define_param);
    fprintf(fp, "#define HIPE_LITERALS_CRC %uU\n", literals_crc);
    fprintf(fp, "#define HIPE_SYSTEM_CRC %uU\n", system_crc);
    fprintf(fp, "#define HIPE_ERTS_CHECKSUM (HIPE_LITERALS_CRC ^ HIPE_SYSTEM_CRC)\n");
    fprintf(fp, "\n");
    fprintf(fp, "#define RTS_PARAMS_CASES");
    print_params(fp, c_case_param);
    fprintf(fp, "\n#endif\n");
    return 0;
}

static int do_e(FILE *fp, const char* this_exe)
{
    fprintf(fp, "%%%% File: hipe_literals.hrl, generated by %s", this_exe);
    fprintf(fp, "\n\n");
    print_literals(fp, e_define_literal);
    fprintf(fp, "\n");
    print_atom_literals(fp, e_define_atom_literal);
    fprintf(fp, "\n");
    print_params(fp, e_define_param);
    fprintf(fp, "\n");
    fprintf(fp, "-define(HIPE_LITERALS_CRC, %u).\n", literals_crc);
    if (is_xcomp) {
	fprintf(fp, "-define(HIPE_SYSTEM_CRC, %u).\n", system_crc);
    }
    else {
	fprintf(fp, "-define(HIPE_SYSTEM_CRC, hipe_bifs:system_crc()).\n");
    }
    fprintf(fp, "-define(HIPE_ERTS_CHECKSUM, (?HIPE_LITERALS_CRC bxor ?HIPE_SYSTEM_CRC)).\n");
    return 0;
}

int main(int argc, const char **argv)
{
    int i;
    int (*do_func_ptr)(FILE *, const char*) = NULL;

    compute_crc();
    for (i = 1; i < argc; i++) {
	if      (strcmp(argv[i], "-c") == 0)
	    do_func_ptr = &do_c;
	else if (strcmp(argv[i], "-e") == 0)
	    do_func_ptr = &do_e;
	else if (strcmp(argv[i], "-x") == 0)
	    is_xcomp = 1;
	else
	    goto error;
    }
    if (do_func_ptr) {
	return do_func_ptr(stdout, argv[0]);
    }
error:
    fprintf(stderr, "usage: %s [-x] [-c | -e] > output-file\n"
	    "\t-c\tC header file\n"
	    "\t-e\tErlang header file\n"
	    "\t-x\tCross compile. No dependencies to compiling emulator\n",	    
	    argv[0]);
    return 1;
}
