/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2009. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * %CopyrightEnd%
 */

/*
** General hash functions
**
*/
#ifndef __HASH_H__
#define __HASH_H__

#ifndef __SYS_H__
#include "sys.h"
#endif

#include "erl_alloc.h"

typedef unsigned long HashValue;

typedef int (*HCMP_FUN)(void*, void*);
typedef HashValue (*H_FUN)(void*);
typedef void* (*HALLOC_FUN)(void*);
typedef void (*HFREE_FUN)(void*);

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
} HashFunctions;

typedef struct {
  char *name;
  int   size;
  int   used;
  int   objs;
  int   depth;
} HashInfo;

typedef struct hash
{
    HashFunctions fun;   /* Function block */
    int is_allocated;    /* 0 iff hash structure is on stack or is static */
    ErtsAlcType_t type;
    char* name;          /* Table name (static string, for debugging) */
    int size;		 /* Number of slots */
    int size20percent;   /* 20 percent of number of slots */
    int size80percent;   /* 80 percent of number of slots */
    int ix;              /* Size index in size table */
    int used;		 /* Number of slots used */
    HashBucket** bucket; /* Vector of bucket pointers (objects) */
} Hash;

Hash* hash_new(ErtsAlcType_t, char*, int, HashFunctions);
Hash* hash_init(ErtsAlcType_t, Hash*, char*, int, HashFunctions);

void  hash_delete(Hash*);
void  hash_get_info(HashInfo*, Hash*);
void  hash_info(int, void *, Hash*);
int   hash_table_sz(Hash *);

void* hash_get(Hash*, void*);
void* hash_put(Hash*, void*);
void* hash_erase(Hash*, void*);
void* hash_remove(Hash*, void*);
void  hash_foreach(Hash*, void (*func)(void *, void *), void *);

void erts_hash_merge(Hash* src, Hash* dst);

#endif
