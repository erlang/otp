/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2023-2023. All Rights Reserved.
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

/* This module contains explicit constants that a debugger can use to better
 * navigate the emulator's data structures, helping us avoid hardcoding
 * constants into scripts. */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "erl_version.h"
#include "erl_map.h"

const int etp_smp_compiled = 1;
const int etp_thread_compiled = 1;
const char etp_erts_version[] = ERLANG_VERSION;
const char etp_otp_release[] = ERLANG_OTP_RELEASE;
const char etp_arch[] = ERLANG_ARCHITECTURE;
#if ERTS_ENABLE_KERNEL_POLL
const int erts_use_kernel_poll = 1;
const int etp_kernel_poll_support = 1;
#else
const int erts_use_kernel_poll = 0;
const int etp_kernel_poll_support = 0;
#endif
#if defined(ARCH_64)
const int etp_arch_bits = 64;
#elif defined(ARCH_32)
const int etp_arch_bits = 32;
#else
# error "Not 64-bit, nor 32-bit arch"
#endif
#ifdef BEAMASM
const int etp_beamasm = 1;
#else
const int etp_beamasm = 0;
#endif
#ifdef DEBUG
const int etp_debug_compiled = 1;
#else
const int etp_debug_compiled = 0;
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
const int etp_lock_count = 1;
#else
const int etp_lock_count = 0;
#endif
#ifdef ERTS_ENABLE_LOCK_CHECK
const int etp_lock_check = 1;
#else
const int etp_lock_check = 0;
#endif
const int etp_endianness = ERTS_ENDIANNESS;
const Eterm etp_ref_header = ERTS_REF_THING_HEADER;
#ifdef ERTS_MAGIC_REF_THING_HEADER
const Eterm etp_magic_ref_header = ERTS_MAGIC_REF_THING_HEADER;
#else
const Eterm etp_magic_ref_header = ERTS_REF_THING_HEADER;
#endif
const Eterm etp_the_non_value = THE_NON_VALUE;
#ifdef TAG_LITERAL_PTR
const Eterm etp_ptr_mask = (~(Eterm)7);
const Eterm etp_tag_literal_ptr = TAG_LITERAL_PTR;
#else
const Eterm etp_ptr_mask = (~(Eterm)3);
const Eterm etp_tag_literal_ptr = 0;
#endif
#ifdef ERTS_HOLE_MARKER
const Eterm etp_hole_marker = ERTS_HOLE_MARKER;
#else
const Eterm etp_hole_marker = 0;
#endif

const Eterm etp_arityval_subtag = ARITYVAL_SUBTAG;
const Eterm etp_big_tag_mask = _BIG_TAG_MASK;
const Eterm etp_big_sign_bit = _BIG_SIGN_BIT;
const Eterm etp_pos_big_subtag = POS_BIG_SUBTAG;
const Eterm etp_neg_big_subtag = NEG_BIG_SUBTAG;
const Eterm etp_ref_subtag = REF_SUBTAG;
const Eterm etp_fun_subtag = FUN_SUBTAG;
const Eterm etp_fun_ref_subtag = FUN_REF_SUBTAG;
const Eterm etp_float_subtag = FLOAT_SUBTAG;
const Eterm etp_bitstring_tag_mask = _BITSTRING_TAG_MASK;
const Eterm etp_heap_bits_subtag = HEAP_BITS_SUBTAG;
const Eterm etp_sub_bits_subtag = SUB_BITS_SUBTAG;
const Eterm etp_bin_ref_subtag = BIN_REF_SUBTAG;
const Eterm etp_map_subtag = MAP_SUBTAG;
const Eterm etp_external_tag_mask = _EXTERNAL_TAG_MASK;
const Eterm etp_external_pid_subtag = EXTERNAL_PID_SUBTAG;
const Eterm etp_external_port_subtag = EXTERNAL_PORT_SUBTAG;
const Eterm etp_external_ref_subtag = EXTERNAL_REF_SUBTAG;

const Eterm etp_tag_header_arityval = _TAG_HEADER_ARITYVAL;
const Eterm etp_tag_header_fun = _TAG_HEADER_FUN;
const Eterm etp_tag_header_pos_big = _TAG_HEADER_POS_BIG;
const Eterm etp_tag_header_neg_big = _TAG_HEADER_NEG_BIG;
const Eterm etp_tag_header_float = _TAG_HEADER_FLOAT;
const Eterm etp_tag_header_ref = _TAG_HEADER_REF;
const Eterm etp_tag_header_bin_ref = _TAG_HEADER_BIN_REF;
const Eterm etp_tag_header_heap_bits = _TAG_HEADER_HEAP_BITS;
const Eterm etp_tag_header_sub_bits = _TAG_HEADER_SUB_BITS;
const Eterm etp_tag_header_external_pid = _TAG_HEADER_EXTERNAL_PID;
const Eterm etp_tag_header_external_port = _TAG_HEADER_EXTERNAL_PORT;
const Eterm etp_tag_header_external_ref = _TAG_HEADER_EXTERNAL_REF;
const Eterm etp_tag_header_map = _TAG_HEADER_MAP;

const Eterm etp_tag_header_mask = _TAG_HEADER_MASK;
const Eterm etp_header_subtag_mask = _HEADER_SUBTAG_MASK;
const Eterm etp_header_arity_offs = _HEADER_ARITY_OFFS;

const Eterm etp_tag_primary_size = _TAG_PRIMARY_SIZE;
const Eterm etp_tag_primary_mask = _TAG_PRIMARY_MASK;
const Eterm etp_tag_primary_header = TAG_PRIMARY_HEADER;
const Eterm etp_tag_primary_list = TAG_PRIMARY_LIST;
const Eterm etp_tag_primary_boxed = TAG_PRIMARY_BOXED;
const Eterm etp_tag_primary_immed1 = TAG_PRIMARY_IMMED1;

const Eterm etp_tag_immed1_size = _TAG_IMMED1_SIZE;
const Eterm etp_tag_immed1_mask = _TAG_IMMED1_MASK;
const Eterm etp_tag_immed1_pid = _TAG_IMMED1_PID;
const Eterm etp_tag_immed1_port = _TAG_IMMED1_PORT;
const Eterm etp_tag_immed1_immed2 = _TAG_IMMED1_IMMED2;
const Eterm etp_tag_immed1_small = _TAG_IMMED1_SMALL;

const Eterm etp_tag_immed2_size = _TAG_IMMED2_SIZE;
const Eterm etp_tag_immed2_mask = _TAG_IMMED2_MASK;
const Eterm etp_tag_immed2_atom = _TAG_IMMED2_ATOM;
const Eterm etp_tag_immed2_catch = _TAG_IMMED2_CATCH;
const Eterm etp_tag_immed2_nil = _TAG_IMMED2_NIL;

const Eterm etp_header_map_subtag_mask = _HEADER_MAP_SUBTAG_MASK;
const Eterm etp_header_map_hashmap_head_mask = _HEADER_MAP_HASHMAP_HEAD_MASK;

const Eterm etp_map_subtag_node_bitmap = HAMT_SUBTAG_NODE_BITMAP;
const Eterm etp_map_subtag_head_array = HAMT_SUBTAG_HEAD_ARRAY;
const Eterm etp_map_subtag_head_bitmap = HAMT_SUBTAG_HEAD_BITMAP;
const Eterm etp_map_subtag_head_flatmap = HAMT_SUBTAG_HEAD_FLATMAP;
