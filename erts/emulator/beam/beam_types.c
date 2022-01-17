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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "beam_types.h"

#include "global.h"
#include "erl_message.h"
#include "external.h"

static Sint64 get_sint64(const byte *data);

int beam_types_decode(const byte *data, Uint size, BeamType *out) {
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

    return 1;
}

static Sint64 get_sint64(const byte *data) {
    Sint64 value = 0;
    int i;

    for (i = 0; i < 8; i++) {
        value = value << 8 | (Sint16)data[i];
    }
    return value;
}
