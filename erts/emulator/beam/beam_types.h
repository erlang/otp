/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2021-2024. All Rights Reserved.
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

#define BEAM_TYPES_VERSION 3

#define BEAM_TYPE_NONE               (0)

#define BEAM_TYPE_ATOM               (1 << 0)
#define BEAM_TYPE_BITSTRING          (1 << 1)
#define BEAM_TYPE_CONS               (1 << 2)
#define BEAM_TYPE_FLOAT              (1 << 3)
#define BEAM_TYPE_FUN                (1 << 4)
#define BEAM_TYPE_INTEGER            (1 << 5)
#define BEAM_TYPE_MAP                (1 << 6)
#define BEAM_TYPE_NIL                (1 << 7)
#define BEAM_TYPE_PID                (1 << 8)
#define BEAM_TYPE_PORT               (1 << 9)
#define BEAM_TYPE_REFERENCE          (1 << 10)
#define BEAM_TYPE_TUPLE              (1 << 11)

#define BEAM_TYPE_ANY                ((1 << 12) - 1)

/* This is not a part of the type union proper, but is present in the format
 * to signal the presence of metadata. */
#define BEAM_TYPE_HAS_LOWER_BOUND    (1 << 12)
#define BEAM_TYPE_HAS_UPPER_BOUND    (1 << 13)
#define BEAM_TYPE_HAS_UNIT           (1 << 14)

#define BEAM_TYPE_METADATA_MASK      (BEAM_TYPE_HAS_LOWER_BOUND | \
                                      BEAM_TYPE_HAS_UPPER_BOUND | \
                                      BEAM_TYPE_HAS_UNIT)

typedef struct {
    /** @brief A set of the possible types (atom, tuple, etc) this term may
     * be. When a single bit is set, the term will always be of that type. */
    int type_union;

    /** @brief A set of metadata presence flags, BEAM_TYPE_HAS_XYZ. */
    int metadata_flags;

    /** @brief Minimum numerical value. Only valid when the
     * BEAM_TYPE_HAS_LOWER_BOUND metadata flag is present. */
    Sint64 min;

    /** @brief Maximum numerical value. Only valid when the
     * BEAM_TYPE_HAS_UPPER_BOUND metadata flag is present. */
    Sint64 max;

    /** @brief Unit for bitstring size. Only valid when the BEAM_TYPE_HAS_UNIT
     * metadata flag is present. */
    byte size_unit;
} BeamType;

int beam_types_decode_type(const byte *data, BeamType *out);
void beam_types_decode_extra(const byte *data, BeamType *out);

#endif
