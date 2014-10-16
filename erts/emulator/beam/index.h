/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2013. All Rights Reserved.
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
** General hash and index functions
** The idea behind this file was to capture the
** way Atom,Export and Module table was implemented
*/
#ifndef __INDEX_H__
#define __INDEX_H__

#ifndef __HASH_H__
#include "hash.h"
#endif

typedef struct index_slot 
{
    HashBucket bucket;
    int index;
} IndexSlot;


typedef struct index_table
{
    Hash htable;		/* Mapping obj -> index */
    ErtsAlcType_t type;
    int size;			/* Allocated size */
    int limit;			/* Max size */
    int entries;		/* Number of entries */
    IndexSlot*** seg_table;	/* Mapping index -> obj */
} IndexTable;

#define INDEX_PAGE_SHIFT 10
#define INDEX_PAGE_SIZE (1 << INDEX_PAGE_SHIFT)
#define INDEX_PAGE_MASK ((1 << INDEX_PAGE_SHIFT)-1)

IndexTable *erts_index_init(ErtsAlcType_t, IndexTable *, const char*,
                            int, int, HashFunctions);
void index_info(int, const void* to_arg, const IndexTable*);
int index_table_sz(IndexTable *);

int index_get(const IndexTable*, const void*);

IndexSlot* index_put_entry(IndexTable*, void*);
void erts_index_merge(Hash*, IndexTable*);

/* Erase all entries with index 'ix' and higher
*/
void index_erase_latest_from(IndexTable*, Uint ix);

ERTS_GLB_INLINE int index_put(IndexTable*, void*);
ERTS_GLB_INLINE IndexSlot* erts_index_lookup(const IndexTable*, Uint);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE int index_put(IndexTable* t, void* tmpl)
{
    return index_put_entry(t, tmpl)->index;
}

ERTS_GLB_INLINE IndexSlot*
erts_index_lookup(const IndexTable* t, Uint ix)
{
    return t->seg_table[ix>>INDEX_PAGE_SHIFT][ix&INDEX_PAGE_MASK];
}
#endif

#endif
