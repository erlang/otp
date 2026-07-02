/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2026. All Rights Reserved.
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

#ifndef ERL_MD5_H__
#define ERL_MD5_H__

#include <stdint.h>

#define MD5_DIGEST_LENGTH 16

typedef struct {
    uint32_t a, b, c, d;
    uint64_t tot_len;
    size_t len;
    uint8_t buf[64];
} erts_md5_state;

void erts_md5(const uint8_t* msg, size_t msg_len, uint8_t* out);
void erts_md5_init(erts_md5_state*);
void erts_md5_update(erts_md5_state*, const uint8_t* msg, size_t msg_len);
void erts_md5_finish(uint8_t* out, erts_md5_state*);

#endif // ERL_MD5_H__

