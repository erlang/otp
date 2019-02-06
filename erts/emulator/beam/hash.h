/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
** General hash functions
**
*/
#ifndef __HASH_H__
#define __HASH_H__

#include "sys.h"

typedef unsigned long HashValue;
typedef struct hash Hash;

typedef int (*HCMP_FUN)(void*, void*);
typedef HashValue (*H_FUN)(void*);
typedef void* (*HALLOC_FUN)(void*);
typedef void (*HFREE_FUN)(void*);
/* Meta functions */
typedef void* (*HMALLOC_FUN)(int,size_t);
typedef void (*HMFREE_FUN)(int,void*);
typedef int (*HMPRINT_FUN)(fmtfn_t,void*,char*, ...);

/*
** This bucket must be placed in top of 
** every object that uses hashing!!!
** (Object*) == (Object*) &bucket
*/
typedef struct hash_bucket
{
    struct hash_bucket* next;	/* Next bucket */
    HashValue hvalue;           /* Store hash value for get, rehash */
} HashBucket;

typedef struct hash_functions
{
    H_FUN hash;
    HCMP_FUN cmp;
    HALLOC_FUN alloc;
    HFREE_FUN free;
    HMALLOC_FUN meta_alloc;
    HMFREE_FUN meta_free;
    HMPRINT_FUN meta_print;
} HashFunctions;

typedef struct {
  char *name;
  int   size;
  int   used;
  int   objs;
  int   depth;
} HashInfo;

struct hash
{
    HashFunctions fun;   /* Function block */
    int is_allocated;    /* 0 iff hash structure is on stack or is static */
    int meta_alloc_type; /* argument to pass to meta_alloc and meta_free */
    char* name;          /* Table name (static string, for debugging) */
    int size;		 /* Number of slots */
    int shrink_threshold;
    int grow_threshold;
    int size_ix;         /* Size index in size table */
    int min_size_ix;     /* Never shrink table smaller than this */
    int nobjs;		 /* Number of objects in table */
    HashBucket** bucket; /* Vector of bucket pointers (objects) */
};

Hash* hash_new(int, char*, int, HashFunctions);
Hash* hash_init(int, Hash*, char*, int, HashFunctions);

void  hash_delete(Hash*);
void  hash_get_info(HashInfo*, Hash*);
void  hash_info(fmtfn_t, void *, Hash*);
int   hash_table_sz(Hash *);

void* hash_get(Hash*, void*);
void* hash_put(Hash*, void*);
void* hash_erase(Hash*, void*);
void* hash_remove(Hash*, void*);
void  hash_foreach(Hash*, void (*func)(void *, void *), void *);

#endif
