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
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef ERL_INTERFACE
/*
 * Built by erl_interface with no sys.h
 * Include and define the stuff we need.
 */
#  include <stdint.h>
#  include <stddef.h>
#  include <string.h>
#  define sys_memcpy memcpy
#  define sys_memzero(PTR, N) memset(PTR, 0, N)
#  define put_little_int32(i, s) do {              \
        ((uint8_t*)(s))[3] = (uint8_t)((i) >> 24); \
        ((uint8_t*)(s))[2] = (uint8_t)((i) >> 16); \
        ((uint8_t*)(s))[1] = (uint8_t)((i) >> 8);  \
        ((uint8_t*)(s))[0] = (uint8_t)(i);         \
    } while (0)

#  define get_little_int32(s)                      \
    ((((uint8_t*) (s))[3] << 24) |                 \
     (((uint8_t*) (s))[2] << 16) |                 \
     (((uint8_t*) (s))[1] << 8) |                  \
     (((uint8_t*) (s))[0]))

#  define put_little_int64(i, s) do {                        \
        ((uint8_t*)(s))[7] = (uint8_t)((uint64_t)(i) >> 56); \
        ((uint8_t*)(s))[6] = (uint8_t)((uint64_t)(i) >> 48); \
        ((uint8_t*)(s))[5] = (uint8_t)((uint64_t)(i) >> 40); \
        ((uint8_t*)(s))[4] = (uint8_t)((uint64_t)(i) >> 32); \
        ((uint8_t*)(s))[3] = (uint8_t)((uint64_t)(i) >> 24); \
        ((uint8_t*)(s))[2] = (uint8_t)((uint64_t)(i) >> 16); \
        ((uint8_t*)(s))[1] = (uint8_t)((uint64_t)(i) >> 8);  \
        ((uint8_t*)(s))[0] = (uint8_t)((uint64_t)(i));       \
    } while (0)

#  include "eidef.h"
#else
#  include "sys.h"
#endif

#include "erl_md5.h"

#define A0 0x67452301
#define B0 0xefcdab89
#define C0 0x98badcfe
#define D0 0x10325476

static size_t pad(const uint8_t* last_part, size_t size, uint8_t pad_buf[],
                  size_t tot_size);
static void hash_part(const uint8_t* msg_part, erts_md5_state*);

void erts_md5_init(erts_md5_state* state)
{
    state->a = A0;
    state->b = B0;
    state->c = C0;
    state->d = D0;
    state->len = 0;
    state->tot_len = 0;
    sys_memzero(state->buf, sizeof(state->buf));
}

void erts_md5_update(erts_md5_state* state, const uint8_t* msg, size_t msg_len)
{
    const uint8_t* part = msg;
    size_t left = msg_len;

    if (state->len > 0) {
        uint8_t* buf = state->buf;
        size_t copy_size;
        buf += state->len;
        copy_size = 64-state->len;
        if (copy_size < left) {
            sys_memcpy(buf, part, copy_size);
            part += copy_size;
            left -= copy_size;
            hash_part(state->buf, state);
            state->len = 0;
            state->tot_len += 64;
        } else {
            sys_memcpy(buf, part, left);
            state->len += left;
            return;
        }
    }

    while (left > 64) {
        hash_part(part, state);
        left -= 64;
        part += 64;
        state->tot_len += 64;
    }

    if (left > 0) {
        sys_memcpy(state->buf, part, left);
        state->len = left;
    }
}

void erts_md5_finish(uint8_t* out, erts_md5_state* state)
{
    uint8_t pad_buf[2*64];
    const uint8_t* part = state->buf;
    size_t left = state->len;

    left = pad(part, left, pad_buf, state->tot_len+state->len);
    part = pad_buf;

    while (left > 0) {
        hash_part(part, state);
        left -= 64;
        part += 64;
    }

    put_little_int32(state->a, out);
    put_little_int32(state->b, out+4);
    put_little_int32(state->c, out+8);
    put_little_int32(state->d, out+12);
}

void erts_md5(const uint8_t* msg, size_t msg_len, uint8_t* out)
{
    erts_md5_state state;

    erts_md5_init(&state);
    erts_md5_update(&state, msg, msg_len);
    erts_md5_finish(out, &state);
}


static void hash_part(const uint8_t* msg_part, erts_md5_state *state)
{
    static const struct {
        uint32_t shift;
        uint32_t k;
    } sk[] =
    {{ 7, 0xd76aa478}, {12, 0xe8c7b756}, {17, 0x242070db}, {22, 0xc1bdceee},
     { 7, 0xf57c0faf}, {12, 0x4787c62a}, {17, 0xa8304613}, {22, 0xfd469501},
     { 7, 0x698098d8}, {12, 0x8b44f7af}, {17, 0xffff5bb1}, {22, 0x895cd7be},
     { 7, 0x6b901122}, {12, 0xfd987193}, {17, 0xa679438e}, {22, 0x49b40821},
     { 5, 0xf61e2562}, { 9, 0xc040b340}, {14, 0x265e5a51}, {20, 0xe9b6c7aa},
     { 5, 0xd62f105d}, { 9, 0x02441453}, {14, 0xd8a1e681}, {20, 0xe7d3fbc8},
     { 5, 0x21e1cde6}, { 9, 0xc33707d6}, {14, 0xf4d50d87}, {20, 0x455a14ed},
     { 5, 0xa9e3e905}, { 9, 0xfcefa3f8}, {14, 0x676f02d9}, {20, 0x8d2a4c8a},
     { 4, 0xfffa3942}, {11, 0x8771f681}, {16, 0x6d9d6122}, {23, 0xfde5380c},
     { 4, 0xa4beea44}, {11, 0x4bdecfa9}, {16, 0xf6bb4b60}, {23, 0xbebfbc70},
     { 4, 0x289b7ec6}, {11, 0xeaa127fa}, {16, 0xd4ef3085}, {23, 0x04881d05},
     { 4, 0xd9d4d039}, {11, 0xe6db99e5}, {16, 0x1fa27cf8}, {23, 0xc4ac5665},
     { 6, 0xf4292244}, {10, 0x432aff97}, {15, 0xab9423a7}, {21, 0xfc93a039},
     { 6, 0x655b59c3}, {10, 0x8f0ccc92}, {15, 0xffeff47d}, {21, 0x85845dd1},
     { 6, 0x6fa87e4f}, {10, 0xfe2ce6e0}, {15, 0xa3014314}, {21, 0x4e0811a1},
     { 6, 0xf7537e82}, {10, 0xbd3af235}, {15, 0x2ad7d2bb}, {21, 0xeb86d391}};

    uint32_t a = state->a;
    uint32_t b = state->b;
    uint32_t c = state->c;
    uint32_t d = state->d;

    for (int i = 0; i < 64; i++) {
        uint32_t f;
        int g;
        const uint8_t *word_ptr;

        if (i <= 15) {
            f = d ^ (b & (c ^ d));
            g = i;
        }
        else if (i <= 31) {
            f = c ^ (d & (b ^ c));
            g = (5*i + 1) & 0xF;
        }
        else if (i <= 47) {
            f = b ^ c ^ d;
            g = (3*i + 5) & 0xF;
        }
        else {
            f = c ^ (b | ~d);
            g = (7*i) & 0xF;
        }
        word_ptr = msg_part + g*4;
        f += (a + sk[i].k + get_little_int32(word_ptr));
        f = (f << sk[i].shift) | (f >> (32 - sk[i].shift));

        a = d;
        d = c;
        c = b;
        b += f;
    }

    state->a += a;
    state->b += b;
    state->c += c;
    state->d += d;
}


static size_t pad(const uint8_t* last_part, size_t size, uint8_t pad_buf[],
                  size_t tot_size)
{
    size_t filled = (size + 1) % 64;
    size_t zeroes;
    const uint64_t tot_bit_size = tot_size * 8;
    uint8_t* pad_ptr = pad_buf;

    if (filled > (64-8)) {
        zeroes = 2*64 - 8 - filled;
    }
    else {
        zeroes = 64 - 8 - filled;
    }

    sys_memcpy(pad_ptr, last_part, size);
    pad_ptr += size;
    *pad_ptr++ = 128;
    sys_memzero(pad_ptr, zeroes);
    pad_ptr += zeroes;
    put_little_int64(tot_bit_size, pad_ptr);
    pad_ptr += 8;

    ASSERT((pad_ptr - pad_buf) % 64 == 0);

    return pad_ptr - pad_buf;
}

