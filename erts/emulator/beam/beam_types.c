/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2021-2023. All Rights Reserved.
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

static Sint64 get_sint64(const byte *data);

int beam_types_decode_type_otp_26(const byte *data, BeamType *out) {
    int types;
    int extra = 0;

    types = (Uint16)data[0] << 8 | (Uint16)data[1];
    if (types == BEAM_TYPE_NONE) {
        return -1;
    }

    /* The "extra data" aren't part of the type union proper. */
    out->type_union = types & BEAM_TYPE_ANY;

    if (types & BEAM_TYPE_HAS_LOWER_BOUND) {
        extra += 8;
    }

    if (types & BEAM_TYPE_HAS_UPPER_BOUND) {
        extra += 8;
    }

    if (types & BEAM_TYPE_HAS_UNIT) {
        extra += 1;
    }

    return extra;
}

void beam_types_decode_extra_otp_26(const byte *data, BeamType *out) {
    int types = out->type_union;
    out->type_union = types & BEAM_TYPE_ANY;

    out->min = MAX_SMALL + 1;
    out->max = MIN_SMALL - 1;
    out->size_unit = 1;

    if (types & BEAM_TYPE_HAS_LOWER_BOUND) {
        out->min = get_sint64(data);
        data += 8;
    }

    if (types & BEAM_TYPE_HAS_UPPER_BOUND) {
        out->max = get_sint64(data);
        data += 8;
    }

    if (types & BEAM_TYPE_HAS_UNIT) {
        out->size_unit = data[0] + 1;
    }
}

int beam_types_decode_otp_25(const byte *data, Uint size, BeamType *out) {
    int types;

    if (size != 18) {
        return 0;
    }

    types = (Uint16)data[0] << 8 | (Uint16)data[1];
    if (types == BEAM_TYPE_NONE) {
        return 0;
    }

    out->type_union = types;

    data += 2;
    out->min = get_sint64(data);
    data += 8;
    out->max = get_sint64(data);

    if (out->min > out->max) {
        out->min = MAX_SMALL + 1;
        out->max = MIN_SMALL - 1;
    }

    out->size_unit = 1;

    return 1;
}

static Sint64 get_sint64(const byte *data) {
    Sint64 value = 0;
    int i;

    for (i = 0; i < 8; i++) {
        value = value << 8 | (Sint64)data[i];
    }
    return value;
}
