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
** Interface to elib statistics
**
*/
#ifndef __ELIB_STAT_H__
#define __ELIB_STAT_H__

struct elib_stat {
    int mem_total;    /* Number of heap words  */
    int mem_blocks;   /* Number of block */
    int mem_alloc;    /* Number of words in use */
    int mem_free;     /* Number of words free */
    int min_used;     /* Size of the smallest block used */
    int max_free;     /* Size of the largest free block */
    int free_blocks;  /* Number of fragments in free list */
    int mem_max_alloc;/* Max number of words in use */
};

EXTERN_FUNCTION(void, elib_statistics, (void*));
EXTERN_FUNCTION(int,  elib_check_heap, (_VOID_));
EXTERN_FUNCTION(void, elib_heap_dump, (char*));
EXTERN_FUNCTION(void, elib_stat, (struct elib_stat*));
EXTERN_FUNCTION(int,  elib_heap_map, (unsigned char*, int));
EXTERN_FUNCTION(int,  elib_histo, (unsigned long*, unsigned long*, int, int));

#endif
