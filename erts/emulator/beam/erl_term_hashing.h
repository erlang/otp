/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2022-2023. All Rights Reserved.
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

#ifndef ERL_TERM_HASHING_H__
#define ERL_TERM_HASHING_H__

#include "sys.h"
#include "erl_drv_nif.h"

#if (defined(__aarch64__) && defined(__ARM_FEATURE_CRC32)) ||                 \
    (defined(__x86_64__) && defined(__SSE4_2__))
#   define ERL_INTERNAL_HASH_CRC32C
#endif

typedef struct {
    Uint32 a,b,c;
} ErtsBlockHashHelperCtx;

typedef struct {
    ErtsBlockHashHelperCtx hctx;
    const byte *ptr;
    Uint len;
    Uint tot_len;
} ErtsBlockHashState;

typedef struct {
    ErtsBlockHashHelperCtx hctx;
    SysIOVec* iov;
    Uint vlen;
    Uint tot_len;
    int vix;
    int ix;
} ErtsIovBlockHashState;

Uint32 make_hash2(Eterm);
Uint32 trapping_make_hash2(Eterm, Eterm*, struct process*);
Uint32 make_hash(Eterm);
Uint32 make_internal_hash(Eterm, Uint32 salt);
#ifdef DEBUG
#  define DBG_HASHMAP_COLLISION_BONANZA
#endif
#ifdef DBG_HASHMAP_COLLISION_BONANZA
Uint32 erts_dbg_hashmap_collision_bonanza(Uint32 hash, Eterm key);
#endif
Uint32 make_map_hash(Eterm key);
void erts_block_hash_init(ErtsBlockHashState *state,
                          const byte *ptr,
                          Uint len,
                          Uint32 initval);
int erts_block_hash(Uint32 *hashp,
                    Uint *sizep,
                    ErtsBlockHashState *state);
void erts_iov_block_hash_init(ErtsIovBlockHashState *state,
                              SysIOVec *iov,
                              Uint vlen,
                              Uint32 initval);
int erts_iov_block_hash(Uint32 *hashp,
                        Uint *sizep,
                        ErtsIovBlockHashState *state);


#endif
