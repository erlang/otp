/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2022-2026. All Rights Reserved.
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

/* Internal hash routines that can be changed at will. */

typedef UWord erts_ihash_t;

erts_ihash_t erts_internal_salted_hash(Eterm term, erts_ihash_t salt);
erts_ihash_t erts_internal_hash(Eterm term);
erts_ihash_t erts_map_hash(Eterm term);
ERTS_GLB_INLINE Uint64 erts_ihash_mix64(Uint64 input);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

/* MurmurHash3 fmix64 finalizer, the immediate path of erts_internal_hash. */
ERTS_INLINE Uint64 erts_ihash_mix64(Uint64 input)
{
    Uint64 hash = input;

    hash ^= hash >> 33;
    hash *= 0xFF51AFD7ED558CCDull;
    hash ^= hash >> 33;
    hash *= 0xC4CEB9FE1A85EC53ull;
    hash ^= hash >> 33;

    /* Inverse, if needed for testing. The constants are the modular inverse of
     * the ones above (over 1 << 64).
     *
     * hash ^= hash >> 33;
     * hash *= 0x9CB4B2F8129337DBull;
     * hash ^= hash >> 33;
     * hash *= 0x4F74430C22A54005ull;
     * hash ^= hash >> 33; */

    return hash;
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#ifdef DEBUG
#  define DBG_HASHMAP_COLLISION_BONANZA
#endif

#ifdef DBG_HASHMAP_COLLISION_BONANZA
erts_ihash_t erts_dbg_hashmap_collision_bonanza(erts_ihash_t hash, Eterm key);
#endif

/* Portable hash routines whose results should be bug-compatible across
 * versions. */

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
    Uint vix;
    Uint ix;
} ErtsIovBlockHashState;

Uint32 make_hash2(Eterm);
Uint32 trapping_make_hash2(Eterm, Eterm*, struct process*);
Uint32 make_hash(Eterm);

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
