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

#ifndef ERL_MEMORY_TRACE_BLOCK_TABLE_H__
#define ERL_MEMORY_TRACE_BLOCK_TABLE_H__

#include <stdlib.h>
#include "erl_fixed_size_int_types.h"
#include "erl_memory_trace_parser.h"


#define EMTBT_ALLOC_XBLK_ERROR		(EMTP_MIN_ERROR - 1)
#define EMTBT_REALLOC_NOBLK_ERROR	(EMTP_MIN_ERROR - 2)
#define EMTBT_REALLOC_XBLK_ERROR	(EMTP_MIN_ERROR - 3)
#define EMTBT_REALLOC_BLK_TYPE_MISMATCH	(EMTP_MIN_ERROR - 4)
#define EMTBT_FREE_NOBLK_ERROR		(EMTP_MIN_ERROR - 5)
#define EMTBT_FREE_BLK_TYPE_MISMATCH	(EMTP_MIN_ERROR - 6)
#define EMTBT_INTERNAL_ERROR		(EMTP_MIN_ERROR - 7)

#define EMTBT_MIN_ERROR			EMTBT_INTERNAL_ERROR


typedef struct emtbt_block_ {

    struct emtbt_block_ *	next;
    struct emtbt_block_ *	prev;
    usgnd_int_32		hash;
    struct emtbt_block_ **	bucket;

    struct {
	usgnd_int_32		secs;
	usgnd_int_32		usecs;
    } time;
    usgnd_int_16		type;
    usgnd_int_max		pointer;
    usgnd_int_max		size;
} emtbt_block;

typedef struct emtbt_table_ emtbt_table;

const char *emtbt_error_string(int);
emtbt_table *emtbt_new_table(int,
			     void * (*)(size_t),
			     void * (*)(void *, size_t),
			     void   (*)(void *));
void emtbt_destroy_table(emtbt_table *);
int emtbt_alloc_op(emtbt_table *tab, emtp_operation *op);
int emtbt_realloc_op(emtbt_table *, emtp_operation *, emtbt_block *);
int emtbt_free_op(emtbt_table *, emtp_operation *, emtbt_block *);

#endif
