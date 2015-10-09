/* ``Licensed under the Apache License, Version 2.0 (the "License");
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
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */


/*
 * Description:	
 *
 * Author: 	Rickard Green
 */

/* Headers to include ... */

#ifdef HAVE_CONFIG_H
#	include "config.h"
#endif

#include "erl_memory_trace_block_table.h"
#include <errno.h>

#undef HARD_DEBUG
#undef REALLY_HARD_DEBUG
#ifdef DEBUG
#  define HARD_DEBUG 0
#  define REALLY_HARD_DEBUG 0
#else
#  define HARD_DEBUG 0
#  define REALLY_HARD_DEBUG 0
#endif

/* Some system specific defines ... */
#if defined(__WIN32__) && !defined(__GNUC__)
#	define INLINE __forceinline
#else
#	ifdef __GNUC__
#		define INLINE __inline__
#	else
#		define INLINE
#	endif
#endif

/* Our own assert() ... */
#ifdef DEBUG
#define ASSERT(A) ((void) ((A) ? 1 : assert_failed(__FILE__, __LINE__, #A)))
#include <stdio.h>
static int assert_failed(char *f, int l, char *a)
{
    fprintf(stderr, "%s:%d: Assertion failed: %s\n", f, l, a);
    abort();
    return 0;
}

#else
#define ASSERT(A) ((void) 1)
#endif


#define EMTBT_BLOCKS_PER_POOL 1000

typedef struct emtbt_block_pool_ {
    struct emtbt_block_pool_ *next;
    emtbt_block blocks[1];
} emtbt_block_pool;

struct emtbt_table_ {
    void * (*alloc)(size_t);
    void * (*realloc)(void *, size_t);
    void   (*free)(void *);
    int is_64_bit;
    int no_blocks;
    int no_of_buckets;
    int max_used_buckets;
    int min_used_buckets;
    int used_buckets;
    int current_size_index;
    emtbt_block *blocks;
    emtbt_block ** buckets;


    /* Fixed size allocation of blocks */
    emtbt_block_pool *block_pools;
    emtbt_block *free_blocks;
    int blocks_per_pool;

};


static emtbt_block null_blk = {0};

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Block table                                                             *
 *                                                                         *
\*                                                                         */

#if HARD_DEBUG
static void check_table(emtbt_table *table);
#endif

static emtbt_block *
block_alloc_new_pool(emtbt_table *tab)
{
    size_t size;
    emtbt_block_pool *poolp;

    size = sizeof(emtbt_block_pool) - sizeof(emtbt_block);
    size += tab->blocks_per_pool*sizeof(emtbt_block);

    poolp = (*tab->alloc)(size);

    if (poolp) {
	int i;
	emtbt_block *blks;

	poolp->next = tab->block_pools;
	tab->block_pools = poolp;

	blks = (emtbt_block *) poolp->blocks;

	for (i = 1; i < tab->blocks_per_pool - 1; i++)
	    blks[i].next = &blks[i + 1];
	blks[tab->blocks_per_pool - 1].next = NULL;
	tab->free_blocks = &blks[1];

	return &blks[0];
    }
    return NULL;
}

static INLINE emtbt_block *
block_alloc(emtbt_table *tab)
{
    emtbt_block *res;
#if HARD_DEBUG
    check_table(tab);
#endif

    if (tab->free_blocks) {
	res = tab->free_blocks;
	tab->free_blocks = tab->free_blocks->next;
    }
    else {
	res = block_alloc_new_pool(tab);
    }

#ifdef DEBUG
    res->next = ((emtbt_block *) 0xfffffff0);
    res->prev = ((emtbt_block *) 0xfffffff0);
    res->bucket = ((emtbt_block **) 0xfffffff0);
#endif

#if HARD_DEBUG
    check_table(tab);
#endif

    return res;
}

static INLINE void
block_free(emtbt_table *tab, emtbt_block *bp)
{

#if HARD_DEBUG
    check_table(tab);
#endif

    bp->next = tab->free_blocks;
    tab->free_blocks = bp;

#if HARD_DEBUG
    check_table(tab);
#endif


}

#define PRIME0 ((usgnd_int_32) 268438039)
#define PRIME1 ((usgnd_int_32) 268440479)
#define PRIME2 ((usgnd_int_32) 268439161)
#define PRIME3 ((usgnd_int_32) 268437017)

#define MK_HASH(H, P, IS64)						\
do {									\
    (H) = (P) & 0xff;							\
    (H) *= PRIME0;							\
    (H) += ((P) >> 8) & 0xff;						\
    (H) *= PRIME1;							\
    (H) += ((P) >> 16) & 0xff;						\
    (H) *= PRIME2;							\
    (H) += ((P) >> 24) & 0xff;						\
    (H) *= PRIME3;							\
    if ((IS64)) {							\
	(H) += ((P) >> 32) & 0xff;					\
	(H) *= PRIME0;							\
	(H) += ((P) >> 40) & 0xff;					\
	(H) *= PRIME1;							\
	(H) += ((P) >> 48) & 0xff;					\
	(H) *= PRIME2;							\
	(H) += ((P) >> 56) & 0xff;					\
	(H) *= PRIME3;							\
    }									\
} while (0)

static const int table_sizes[] = {
    3203,
    4813,
    6421,
    9643,
    12853,
    19289,
    25717,
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
    52679969
};

#if HARD_DEBUG

static void
check_table(emtbt_table *table)
{
    int no_blocks;
    emtbt_block *block, *prev_block;

    no_blocks = 0;
    block = table->blocks;
    ASSERT(!block || !block->prev);
    prev_block = NULL;
    while (block) {
	usgnd_int_32 hash;
	MK_HASH(hash, block->pointer, table->is_64_bit);
	ASSERT(hash == block->hash);
	ASSERT(block->bucket - table->buckets
	       == hash % table->no_of_buckets);
	ASSERT(!prev_block || prev_block == block->prev);
	prev_block = block;
	block = block->next;
	no_blocks++;
	ASSERT(table->no_blocks >= no_blocks);
    }

    ASSERT(table->no_blocks == no_blocks);

#if REALLY_HARD_DEBUG
    {
	int i;
	for (i = 0; i < table->no_of_buckets; i++) {
	    int bucket_end_found;
	    emtbt_block **bucket;
	    if (!table->buckets[i])
		continue;
	    bucket_end_found = 0;
	    bucket = &table->buckets[i];
	    for (block = table->blocks; block; block = block->next) {
		if (block->bucket == bucket) {
		    if (!block->prev || block->prev->bucket != bucket)
			ASSERT(*bucket == block);
		    if (!block->next || block->next->bucket != bucket)
			bucket_end_found++;
		}
	    }
	    ASSERT(bucket_end_found);
	}
    }
#endif

}

#endif

static INLINE void
link_block(emtbt_table *table, emtbt_block **bucket, emtbt_block *block)
{
    ASSERT(bucket);

    block->bucket = bucket;
    if (*bucket) {
	block->next = *bucket;
	block->prev = (*bucket)->prev;
	if (block->prev)
	    block->prev->next = block;
	else
	    table->blocks = block;
	block->next->prev = block;
    }
    else {
	block->next = table->blocks;
	block->prev = NULL;
	if (table->blocks)
	    table->blocks->prev = block;
	table->blocks = block;
	table->used_buckets++;

    }
    *bucket = block;
    table->no_blocks++;

#if HARD_DEBUG
    check_table(table);
#endif

}

static int
resize_table(emtbt_table *table, int new_no_of_buckets)
{
#ifdef DEBUG
    int org_no_blocks;
#endif
    int i;
    emtbt_block *block;
    emtbt_block **buckets;

    if (new_no_of_buckets < table->no_of_buckets) {
	/* shrink never fails */
	buckets = (emtbt_block **) (*table->realloc)(table->buckets,
						     (sizeof(emtbt_block *)
						      * new_no_of_buckets));
	if (!buckets)
	    return 1;
    }
    else if (new_no_of_buckets > table->no_of_buckets) {
	(*table->free)((void *) table->buckets);
	buckets = (emtbt_block **) (*table->alloc)((sizeof(emtbt_block *)
						    * new_no_of_buckets));
	if (!buckets)
	    return 0;
    }
    else
	return 1;

    table->buckets = buckets;
    table->no_of_buckets = new_no_of_buckets;
    table->max_used_buckets = (4*new_no_of_buckets)/5;
    table->min_used_buckets = new_no_of_buckets/5;
    table->used_buckets = 0;

#ifdef DEBUG
    org_no_blocks = table->no_blocks;
#endif

    table->no_blocks = 0;
    

    for (i = 0; i < new_no_of_buckets; i++)
	buckets[i] = NULL;

    block = table->blocks;
    table->blocks = NULL;

    while (block) {
	emtbt_block *next_block = block->next;
	link_block(table,&table->buckets[block->hash%new_no_of_buckets],block);
	block = next_block;
    }

    ASSERT(org_no_blocks == table->no_blocks);

    return 1;
}

static INLINE int
grow_table(emtbt_table *table)
{
    if (table->current_size_index < sizeof(table_sizes)/sizeof(int)) {
	int new_size;
	table->current_size_index++;
	new_size = table_sizes[table->current_size_index];
	ASSERT(new_size > 0);
	return resize_table(table, new_size);
    }
    return 1;
}

static INLINE void
shrink_table(emtbt_table *table)
{
    if (table->current_size_index > 0) {
	int new_size;
	table->current_size_index--;
	new_size = table_sizes[table->current_size_index];
	ASSERT(new_size > 0);
	(void) resize_table(table, new_size);
    }
}

static INLINE emtbt_block *
peek_block(emtbt_table *table, usgnd_int_max ptr)
{
    emtbt_block **bucket;
    emtbt_block *block;
    usgnd_int_32 hash;

    MK_HASH(hash, ptr, table->is_64_bit);

    bucket = &table->buckets[hash % table->no_of_buckets];
    block = *bucket;
    if (!block)
	return NULL;

    while (block->bucket == bucket) {
	ASSERT(block);
	if (block->pointer == ptr)
	    return block;
	if (!block->next)
	    break;
	block = block->next;
    }
    return NULL;
}

static INLINE int
insert_block(emtbt_table *table, emtbt_block *block)
{
    emtbt_block **bucket;
    emtbt_block *tmp_block;
    usgnd_int_32 hash;
    usgnd_int_max p;

#if HARD_DEBUG
    check_table(table);
#endif

    if (table->used_buckets >= table->max_used_buckets) {
	if(!grow_table(table))
	    return -1;
    }

    p = block->pointer;

    MK_HASH(hash, p, table->is_64_bit);
    block->hash = hash;

    bucket = &table->buckets[hash % table->no_of_buckets];
    tmp_block = *bucket;
    if (tmp_block) {
	while (tmp_block->bucket == bucket) {
	    if (tmp_block->pointer == p)
		return 0;
	    if (!tmp_block->next)
		break;
	    tmp_block = tmp_block->next;
	}
    }

    link_block(table, bucket, block);

    ASSERT(block == peek_block(table, p));


    return 1;
}

static INLINE void
delete_block(emtbt_table *table, emtbt_block *block)
{
    emtbt_block **bucket;

    if (!block)
	return;

#if HARD_DEBUG
    check_table(table);
#endif

    bucket = block->bucket;
    ASSERT(bucket);

    if (block->prev)
	block->prev->next = block->next;
    else
	table->blocks = block->next;

    if (block->next)
	block->next->prev = block->prev;

    if (block == *bucket) {
	ASSERT(!block->prev || block->prev->bucket != bucket);
	if (block->next && block->next->bucket == bucket)
	    *bucket = block->next;
	else {
	    ASSERT(table->used_buckets > 0);
	    *bucket = NULL;
	    table->used_buckets--;
	}
    }
#ifdef DEBUG

    block->next = ((emtbt_block *) 0xfffffff0);
    block->prev = ((emtbt_block *) 0xfffffff0);
    block->bucket = ((emtbt_block **) 0xfffffff0);
#endif

    ASSERT(table->no_blocks > 0);
    table->no_blocks--;

    if (table->used_buckets < table->min_used_buckets)
	shrink_table(table);

#if HARD_DEBUG
    check_table(table);
#endif

}

static INLINE emtbt_block *
fetch_block(emtbt_table *table, usgnd_int_max ptr)
{
    emtbt_block *block;

    block = peek_block(table, ptr);
    delete_block(table, block);
    return block;
}


const char *emtbt_error_string(int error)
{
    switch (error) {
    case EMTBT_ALLOC_XBLK_ERROR:
	return "Allocation to an already existing block";
    case EMTBT_REALLOC_NOBLK_ERROR:
	return "Reallocation of non-existing block";
    case EMTBT_REALLOC_XBLK_ERROR:
	return "Reallocation to an already existing block";
    case EMTBT_REALLOC_BLK_TYPE_MISMATCH:
	return "Block types mismatch when reallocating";
    case EMTBT_FREE_NOBLK_ERROR:
	return "Deallocation of non-existing block";
    case EMTBT_FREE_BLK_TYPE_MISMATCH:
	return "Block types mismatch when deallocating";
    case EMTBT_INTERNAL_ERROR:
	return "Block table internal error";
    default:
	return NULL;
    }


}


emtbt_table *
emtbt_new_table(int is_64_bit,
		void * (*alloc)(size_t),
		void * (*realloc)(void *, size_t),
		void   (*free)(void *))
{
    emtbt_table *tab = (*alloc)(sizeof(emtbt_table));
    if (tab) {
	tab->alloc = alloc;
	tab->realloc = realloc;
	tab->free = free;
	tab->is_64_bit = is_64_bit;
	tab->no_blocks = 0;
	tab->no_of_buckets = 0;
	tab->max_used_buckets = 0;
	tab->min_used_buckets = 0;
	tab->used_buckets = 0;
	tab->current_size_index = 0;
	tab->blocks = NULL;
	tab->buckets = NULL;

	tab->block_pools = NULL;
	tab->free_blocks = NULL;
	tab->blocks_per_pool = EMTBT_BLOCKS_PER_POOL;

    }
    return tab;
}

void
emtbt_destroy_table(emtbt_table *tab)
{
    void (*freep)(void *);
    emtbt_block_pool *poolp1, *poolp2;

    freep = tab->free;

    /* Free block pools */
    poolp1 = tab->block_pools;
    while (poolp1) {
	poolp2 = poolp1;
	poolp1 = poolp1->next;
	(*freep)((void *) poolp2);
    }

    if (tab->buckets)
	(*freep)((void *) tab->buckets);

    (*freep)((void *) tab);
}


#define CP_BLK(TO, FROM)						\
do {									\
    (TO)->time.secs	= (FROM)->time.secs;				\
    (TO)->time.usecs	= (FROM)->time.usecs;				\
    (TO)->type		= (FROM)->type;					\
    (TO)->pointer	= (FROM)->pointer;				\
    (TO)->size		= (FROM)->size;					\
} while (0)

int
emtbt_alloc_op(emtbt_table *tab, emtp_operation *op)
{
    int res;
    emtbt_block *blk;

    blk = block_alloc(tab);
    if (!blk)
	return ENOMEM;
	
    blk->time.secs	= op->time.secs;
    blk->time.usecs	= op->time.usecs;
    blk->type		= op->u.block.type;
    blk->pointer	= op->u.block.new_ptr;
    blk->size		= op->u.block.new_size;

    res = insert_block(tab, blk);
    if (res < 0)
	return ENOMEM;
    else if (res == 0)
	return EMTBT_ALLOC_XBLK_ERROR;
    return 0;
}

int
emtbt_realloc_op(emtbt_table *tab, emtp_operation *op, emtbt_block *old_blk)
{
    int res;
    emtbt_block *blk;

    if (!op->u.block.new_size) {
	/* freed block */

	blk = fetch_block(tab, op->u.block.prev_ptr);
	if (!blk)
	    return EMTBT_REALLOC_NOBLK_ERROR;

	CP_BLK(old_blk, blk);
	block_free(tab, blk);
    }
    else {

	if (!op->u.block.new_ptr) {
	    /* failed operation */
	    if (!op->u.block.prev_ptr)
		CP_BLK(old_blk, &null_blk);
	    else {
		blk = peek_block(tab, op->u.block.prev_ptr);
		if (!blk)
		    return EMTBT_REALLOC_NOBLK_ERROR;
		CP_BLK(old_blk, blk);
#if 0
		if (blk->type != op->u.block.type)
		    return EMTBT_REALLOC_BLK_TYPE_MISMATCH;
#endif
	    }
	}
	else if (!op->u.block.prev_ptr) {
	    /* new block */

	    CP_BLK(old_blk, &null_blk);
	    blk = block_alloc(tab);
	    if (!blk)
		return ENOMEM;
	    blk->type		= op->u.block.type;
	    blk->pointer	= op->u.block.new_ptr;
	    blk->time.secs	= op->time.secs;
	    blk->time.usecs	= op->time.usecs;
	    blk->size		= op->u.block.new_size;

	    res = insert_block(tab, blk);
	    if (res < 0)
		return ENOMEM;
	    else if (res == 0)
		return EMTBT_REALLOC_XBLK_ERROR;
	}
	else if (op->u.block.new_ptr == op->u.block.prev_ptr) {
	    /* resized block */
	    blk = peek_block(tab, op->u.block.prev_ptr);
	    if (!blk)
		return EMTBT_REALLOC_NOBLK_ERROR;
	    CP_BLK(old_blk, blk);
	    blk->time.secs	= op->time.secs;
	    blk->time.usecs	= op->time.usecs;
	    blk->size		= op->u.block.new_size;
#if 0
	    if (blk->type != op->u.block.type)
		return EMTBT_REALLOC_BLK_TYPE_MISMATCH;
#endif
	}
	else {
	    /* moved block */
	    blk = fetch_block(tab, op->u.block.prev_ptr);
	    if (!blk)
		return EMTBT_REALLOC_NOBLK_ERROR;
	    CP_BLK(old_blk, blk);
	    blk->time.secs	= op->time.secs;
	    blk->time.usecs	= op->time.usecs;
	    blk->pointer 	= op->u.block.new_ptr;
	    blk->size		= op->u.block.new_size;
	    res = insert_block(tab, blk);
	    if (res < 0)
		return ENOMEM;
	    else if (res == 0)
		return EMTBT_REALLOC_XBLK_ERROR;
#if 0
	    if (blk->type != op->u.block.type)
		return EMTBT_REALLOC_BLK_TYPE_MISMATCH;
#endif
	}
    }   
    return 0;

}


int
emtbt_free_op(emtbt_table *tab, emtp_operation *op, emtbt_block *old_blk)
{
    emtbt_block *blk;

    if (!op->u.block.prev_ptr)
	CP_BLK(old_blk, &null_blk);
    else {

	blk = fetch_block(tab, op->u.block.prev_ptr);
	if (!blk)
	    return EMTBT_FREE_NOBLK_ERROR;

	CP_BLK(old_blk, blk);
	block_free(tab, blk);
#if 0
	if (blk->type != op->u.block.type)
	    return EMTBT_FREE_BLK_TYPE_MISMATCH;
#endif
    }
    return 0;
}
