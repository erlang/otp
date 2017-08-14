/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

/*
** General thread safe hash table. Simular interface as hash.h
**
** Author: Sverker Eriksson
*/
#ifndef __SAFE_HASH_H__
#define __SAFE_HASH_H__

#include "sys.h"
#include "erl_alloc.h"
#include "erl_lock_flags.h"

typedef unsigned long SafeHashValue;

typedef int (*SHCMP_FUN)(void*, void*);
typedef SafeHashValue (*SH_FUN)(void*);
typedef void* (*SHALLOC_FUN)(void*);
typedef void (*SHFREE_FUN)(void*);

/*
** This bucket must be placed in top of 
** every object that uses hashing!!!
** (Object*) == (Object*) &bucket
*/
typedef struct safe_hashbucket
{
    struct safe_hashbucket* next;	/* Next bucket */
    SafeHashValue hvalue;           /* Store hash value for get, rehash */
} SafeHashBucket;

typedef struct safe_hashfunctions
{
    SH_FUN hash;
    SHCMP_FUN cmp;
    SHALLOC_FUN alloc;
    SHFREE_FUN free;
} SafeHashFunctions;

typedef struct {
  char *name;
  int   size;
  int   used;
  int   objs;
  int   depth;
} SafeHashInfo;

#define SAFE_HASH_LOCK_CNT 16
typedef struct
{
    SafeHashFunctions fun;    /* (C) Function block */
    ErtsAlcType_t type;       /* (C) */
    char* name;               /* (C) Table name (static, for debugging) */
    int size_mask;	      /* (RW) Number of slots - 1 */
    SafeHashBucket** tab;     /* (RW) Vector of bucket pointers (objects) */
    int grow_limit;           /* (RW) Threshold for growing table */
    erts_atomic_t nitems;       /* (A) Number of items in table */
    erts_atomic_t is_rehashing; /* (A) Table rehashing in progress */

    union {
	erts_mtx_t mtx;
	byte __cache_line__[64];
    }lock_vec[SAFE_HASH_LOCK_CNT];

    /* C: Constants initialized once */
    /* RW: One lock (or is_rehashing) to read and _all_ locks to write */
    /* A: Lockless atomics */
} SafeHash;

SafeHash* safe_hash_init(ErtsAlcType_t, SafeHash*, char*, erts_lock_flags_t, int, SafeHashFunctions);

void  safe_hash_get_info(SafeHashInfo*, SafeHash*);
int   safe_hash_table_sz(SafeHash *);

void* safe_hash_get(SafeHash*, void*);
void* safe_hash_put(SafeHash*, void*);
void* safe_hash_erase(SafeHash*, void*);

void  safe_hash_for_each(SafeHash*, void (*func)(void *, void *, void *), void *, void *);

#ifdef ERTS_ENABLE_LOCK_COUNT
void erts_lcnt_enable_hash_lock_count(SafeHash*, erts_lock_flags_t, int);
#endif

#endif /* __SAFE_HASH_H__ */

