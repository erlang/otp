/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2021-2021. All Rights Reserved.
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

/**
 * @description Basic type representation for BEAM instruction operands.
 * @file beam_types.h
 * 
 * While the compiler is good eliminating redundant type tests and simplifying
 * instructions, we're limited by the available instructions and it's not
 * always worthwhile to add new variants.
 * 
 * The idea behind this module is to allow minor optimizations _inside_
 * instructions based on what we know about their operand types. For example,
 * when we know that the source passed to `is_tagged_tuple` is always boxed, we
 * can skip the boxed check.
 */

#ifndef _BEAM_TYPES_H
#define _BEAM_TYPES_H

#include "sys.h"

#define BEAM_TYPES_VERSION 0

#define BEAM_TYPE_NONE               (0)

#define BEAM_TYPE_ATOM               (1 << 0)
#define BEAM_TYPE_BITSTRING          (1 << 1)
#define BEAM_TYPE_BS_MATCHSTATE      (1 << 2)
#define BEAM_TYPE_CONS               (1 << 3)
#define BEAM_TYPE_FLOAT              (1 << 4)
#define BEAM_TYPE_FUN                (1 << 5)
#define BEAM_TYPE_INTEGER            (1 << 6)
#define BEAM_TYPE_MAP                (1 << 7)
#define BEAM_TYPE_NIL                (1 << 8)
#define BEAM_TYPE_PID                (1 << 9)
#define BEAM_TYPE_PORT               (1 << 10)
#define BEAM_TYPE_REFERENCE          (1 << 11)
#define BEAM_TYPE_TUPLE              (1 << 12)

#define BEAM_TYPE_ANY                ((1 << 13) - 1)

#define BEAM_TYPE_MASK_BOXED        \
    (BEAM_TYPE_BITSTRING |          \
     BEAM_TYPE_BS_MATCHSTATE |      \
     BEAM_TYPE_FLOAT |              \
     BEAM_TYPE_FUN |                \
     BEAM_TYPE_INTEGER |            \
     BEAM_TYPE_MAP |                \
     BEAM_TYPE_PID |                \
     BEAM_TYPE_PORT |               \
     BEAM_TYPE_REFERENCE |          \
     BEAM_TYPE_TUPLE)

#define BEAM_TYPE_MASK_IMMEDIATE    \
    (BEAM_TYPE_ATOM |               \
     BEAM_TYPE_INTEGER |            \
     BEAM_TYPE_NIL |                \
     BEAM_TYPE_PID |                \
     BEAM_TYPE_PORT)

#define BEAM_TYPE_MASK_CELL         \
    (BEAM_TYPE_CONS)

#define BEAM_TYPE_MASK_ALWAYS_IMMEDIATE  \
    (BEAM_TYPE_MASK_IMMEDIATE & ~(BEAM_TYPE_MASK_BOXED | BEAM_TYPE_MASK_CELL))
#define BEAM_TYPE_MASK_ALWAYS_BOXED      \
    (BEAM_TYPE_MASK_BOXED & ~(BEAM_TYPE_MASK_CELL | BEAM_TYPE_MASK_IMMEDIATE))
#define BEAM_TYPE_MASK_ALWAYS_CELL       \
    (BEAM_TYPE_MASK_CELL & ~(BEAM_TYPE_MASK_BOXED | BEAM_TYPE_MASK_IMMEDIATE))

typedef struct {
    /** @brief A set of the possible types (atom, tuple, etc) this term may
     * be. When a single bit is set, the term will always be of that type. */
    int type_union;
} BeamType;

int beam_types_decode(const byte *data, Uint size, BeamType *out);

#endif
