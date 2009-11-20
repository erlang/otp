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
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "hash.h"

/*
** List of sizes (all are primes)
*/
static const int h_size_table[] = {
    2, 5, 11, 23, 47, 97, 197, 397, 797,  /* double upto here */
    1201,   1597,
    2411,   3203, 
    4813,   6421,
    9643,   12853, 
    19289,  25717,
    51437,
    102877,
    205759,
    411527,
    823117,
    1646237,
    3292489,
    6584983,
    13169977,
    26339969,
    52679969, 
    -1 
};

/*
** Get info about hash
**
*/

void hash_get_info(HashInfo *hi, Hash *h)
{
    int size = h->size;
    int i;
    int max_depth = 0;
    int objects = 0;

    for (i = 0; i < size; i++) {
	int depth = 0;
	HashBucket* b = h->bucket[i];
	
	while (b != (HashBucket*) 0) {
	    objects++;
	    depth++;
	    b = b->next;
	}
	if (depth > max_depth)
	    max_depth = depth;
    }

    hi->name  = h->name;
    hi->size  = h->size;
    hi->used  = h->used;
    hi->objs  = objects;
    hi->depth = max_depth;
}

/*
** Display info about hash
**
*/

void hash_info(int to, void *arg, Hash* h)
{
    HashInfo hi;

    hash_get_info(&hi, h);

    erts_print(to, arg, "=hash_table:%s\n", hi.name);
    erts_print(to, arg, "size: %d\n",       hi.size);
    erts_print(to, arg, "used: %d\n",       hi.used);
    erts_print(to, arg, "objs: %d\n",       hi.objs);
    erts_print(to, arg, "depth: %d\n",      hi.depth);
}


/*
 * Returns size of table in bytes. Stored objects not included.
 */
int 
hash_table_sz(Hash *h)
{
  int i;
  for(i=0;h->name[i];i++);
  i++;
  return sizeof(Hash) + h->size*sizeof(HashBucket*) + i;
}


/*
** init a pre allocated or static hash structure
** and allocate buckets.
*/
Hash* hash_init(ErtsAlcType_t type, Hash* h, char* name, int size, HashFunctions fun)
{
    int sz;
    int ix = 0;

    h->type = type;

    while (h_size_table[ix] != -1 && h_size_table[ix] < size)
	ix++;
    if (h_size_table[ix] == -1)
	erl_exit(1, "panic: too large hash table size (%d)\n", size);

    size = h_size_table[ix];
    sz = size*sizeof(HashBucket*);

    h->bucket = (HashBucket**) erts_alloc(h->type, sz);

    sys_memzero(h->bucket, sz);
    h->is_allocated = 0;
    h->name = name;
    h->fun = fun;
    h->size = size;
    h->size20percent = h->size/5;
    h->size80percent = (4*h->size)/5;
    h->ix = ix;
    h->used = 0;
    return h;
}

/*
** Create a new hash table
*/
Hash* hash_new(ErtsAlcType_t type, char* name, int size, HashFunctions fun)
{
    Hash* h;

    h = erts_alloc(type, sizeof(Hash));

    h = hash_init(type, h, name, size, fun);
    h->is_allocated =  1;
    return h;
}

/*
** Delete hash table and all objects
*/
void hash_delete(Hash* h)
{
    int old_size = h->size;
    int i;

    for (i = 0; i < old_size; i++) {
	HashBucket* b = h->bucket[i];
	while (b != (HashBucket*) 0) {
	    HashBucket* b_next = b->next;
	    
	    h->fun.free((void*) b);
	    b = b_next;
	}
    }
    erts_free(h->type, h->bucket);
    if (h->is_allocated)
	erts_free(h->type, (void*) h);
}

/*
** Rehash all objects
*/
static void rehash(Hash* h, int grow)
{
    int sz;
    int old_size = h->size;
    HashBucket** new_bucket;
    int i;

    if (grow) {
	if ((h_size_table[h->ix+1]) == -1)
	    return;
	h->ix++;
    }
    else {
	if (h->ix == 0)
	    return;
	h->ix--;
    }
    h->size = h_size_table[h->ix];
    h->size20percent = h->size/5;
    h->size80percent = (4*h->size)/5;
    sz = h->size*sizeof(HashBucket*);

    new_bucket = (HashBucket **) erts_alloc(h->type, sz);
    sys_memzero(new_bucket, sz);

    h->used = 0;

    for (i = 0; i < old_size; i++) {
	HashBucket* b = h->bucket[i];
	while (b != (HashBucket*) 0) {
	    HashBucket* b_next = b->next;
	    int ix = b->hvalue % h->size;
	    if (new_bucket[ix] == NULL)
		h->used++;
	    b->next = new_bucket[ix];
	    new_bucket[ix] = b;
	    b = b_next;
	}
    }
    erts_free(h->type, (void *) h->bucket);
    h->bucket = new_bucket;
}

/*
** Find an object in the hash table
**
*/
void* hash_get(Hash* h, void* tmpl)
{
    HashValue hval = h->fun.hash(tmpl);
    int ix = hval % h->size;
    HashBucket* b = h->bucket[ix];
	
    while(b != (HashBucket*) 0) {
	if ((b->hvalue == hval) && (h->fun.cmp(tmpl, (void*)b) == 0))
	    return (void*) b;
	b = b->next;
    }
    return (void*) 0;
}

/*
** Find or insert an object in the hash table
*/
void* hash_put(Hash* h, void* tmpl)
{
    HashValue hval = h->fun.hash(tmpl);
    int ix = hval % h->size;
    HashBucket* b = h->bucket[ix];

    while(b != (HashBucket*) 0) {
	if ((b->hvalue == hval) && (h->fun.cmp(tmpl, (void*)b) == 0))
	    return (void*) b;
	b = b->next;
    }
    b = (HashBucket*) h->fun.alloc(tmpl);

    if (h->bucket[ix] == NULL)
	h->used++;

    b->hvalue = hval;
    b->next = h->bucket[ix];
    h->bucket[ix] = b;

    if (h->used > h->size80percent)  /* rehash at 80% */
	rehash(h, 1);
    return (void*) b;
}

static void
hash_insert_entry(Hash* h, HashBucket* entry)
{
    HashValue hval = entry->hvalue;
    int ix = hval % h->size;
    HashBucket* b = h->bucket[ix];

    while (b != (HashBucket*) 0) {
	if ((b->hvalue == hval) && (h->fun.cmp((void*)entry, (void*)b) == 0)) {
	    abort();		/* Should not happen */
	}
	b = b->next;
    }

    if (h->bucket[ix] == NULL)
	h->used++;

    entry->next = h->bucket[ix];
    h->bucket[ix] = entry;

    if (h->used > h->size80percent)  /* rehash at 80% */
	rehash(h, 1);
}


/*
 * Move all entries in src into dst; empty src.
 * Entries in src must not exist in dst.
 */
void
erts_hash_merge(Hash* src, Hash* dst)
{
    int limit = src->size;
    HashBucket** bucket = src->bucket;
    int i;

    src->used = 0;
    for (i = 0; i < limit; i++) {
	HashBucket* b = bucket[i];
	HashBucket* next;

	bucket[i] = NULL;
	while (b) {
	    next = b->next;
	    hash_insert_entry(dst, b);
	    b = next;
	}
    }
}

/*
** Erase hash entry return template if erased
** return 0 if not erased
*/
void* hash_erase(Hash* h, void* tmpl)
{
    HashValue hval = h->fun.hash(tmpl);
    int ix = hval % h->size;
    HashBucket* b = h->bucket[ix];
    HashBucket* prev = 0;
	
    while(b != 0) {
	if ((b->hvalue == hval) && (h->fun.cmp(tmpl, (void*)b) == 0)) {
	    if (prev != 0)
		prev->next = b->next;
	    else
		h->bucket[ix] = b->next;
	    h->fun.free((void*)b);
	    if (h->bucket[ix] == NULL)
		h->used--;
	    if (h->used < h->size20percent)  /* rehash at 20% */
		rehash(h, 0);
	    return tmpl;
	}
	prev = b;
	b = b->next;
    }
    return (void*)0;
}

/*
** Remove hash entry from table return entry if removed
** return NULL if not removed
** NOTE: hash_remove() differs from hash_erase() in that
**       it returns entry (not the template) and does
**       *not* call the free() callback.
*/
void *
hash_remove(Hash *h, void *tmpl)
{
    HashValue hval = h->fun.hash(tmpl);
    int ix = hval % h->size;
    HashBucket *b = h->bucket[ix];
    HashBucket *prev = NULL;
	
    while (b) {
	if ((b->hvalue == hval) && (h->fun.cmp(tmpl, (void*)b) == 0)) {
	    if (prev)
		prev->next = b->next;
	    else
		h->bucket[ix] = b->next;
	    if (h->bucket[ix] == NULL)
		h->used--;
	    if (h->used < h->size20percent)  /* rehash at 20% */
		rehash(h, 0);
	    return (void *) b;
	}
	prev = b;
	b = b->next;
    }
    return NULL;
}

void hash_foreach(Hash* h, void (*func)(void *, void *), void *func_arg2)
{
    int i;

    for (i = 0; i < h->size; i++) {
	HashBucket* b = h->bucket[i];
	while(b != (HashBucket*) 0) {
	    (*func)((void *) b, func_arg2);
	    b = b->next;
	}
    }
}

