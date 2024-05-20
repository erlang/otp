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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "beam_types.h"

#include "global.h"
#include "erl_message.h"
#include "external.h"

static Sint64 get_sint64(const byte *data) {
    Sint64 value = 0;
    int i;

    for (i = 0; i < 8; i++) {
        value = value << 8 | (Sint64)data[i];
    }
    return value;
}


int beam_types_decode_type(const byte *data, BeamType *out) {
    int flags, extra;

    flags = (Uint16)data[0] << 8 | (Uint16)data[1];
    if (flags == BEAM_TYPE_NONE) {
        return -1;
    }

    extra = 0;

    out->type_union = flags & BEAM_TYPE_ANY;
    out->metadata_flags = flags & BEAM_TYPE_METADATA_MASK;

    if (out->metadata_flags & BEAM_TYPE_HAS_LOWER_BOUND) {
        if (!(out->type_union & (BEAM_TYPE_FLOAT | BEAM_TYPE_INTEGER))) {
            return -1;
        }

        extra += 8;
    }

    if (out->metadata_flags & BEAM_TYPE_HAS_UPPER_BOUND) {
        if (!(out->type_union & (BEAM_TYPE_FLOAT | BEAM_TYPE_INTEGER))) {
            return -1;
        }

        extra += 8;
    }

    if (out->metadata_flags & BEAM_TYPE_HAS_UNIT) {
        if (!(out->type_union & BEAM_TYPE_BITSTRING)) {
            return -1;
        }

        extra += 1;
    }

    return extra;
}

void beam_types_decode_extra(const byte *data, BeamType *out) {
    out->min = MAX_SMALL + 1;
    out->max = MIN_SMALL - 1;
    out->size_unit = 1;

    if (out->metadata_flags & BEAM_TYPE_HAS_LOWER_BOUND) {
        ASSERT(out->type_union & (BEAM_TYPE_FLOAT | BEAM_TYPE_INTEGER));
        out->min = get_sint64(data);
        data += 8;
    }

    if (out->metadata_flags & BEAM_TYPE_HAS_UPPER_BOUND) {
        ASSERT(out->type_union & (BEAM_TYPE_FLOAT | BEAM_TYPE_INTEGER));
        out->max = get_sint64(data);
        data += 8;
    }

    if (out->metadata_flags & BEAM_TYPE_HAS_UNIT) {
        ASSERT(out->type_union & BEAM_TYPE_BITSTRING);
        out->size_unit = data[0] + 1;
    }
}
