/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
 * Description:	
 *
 * Author: 	Rickard Green
 */

#ifndef ERL_MTRACE_PARSER_H__
#define ERL_MTRACE_PARSER_H__

#include <stdlib.h>
#include "erl_fixed_size_int_types.h"

/* emtp_parse() return values */
#define EMTP_MIN_ERROR					EMTP_NO_TRACE_ERROR

#define EMTP_NO_TRACE_ERROR				(-11)
#define EMTP_HEADER_TAG_IN_BODY_ERROR			(-10)
#define EMTP_BODY_TAG_IN_HEADER_ERROR			( -9)
#define EMTP_NOT_SUPPORTED_MTRACE_VERSION_ERROR		( -8)
#define EMTP_NOT_AN_ERL_MTRACE_ERROR			( -7)
#define EMTP_NO_MEMORY_ERROR				( -6)
#define EMTP_BAD_OP_SIZE_ERROR				( -5)
#define EMTP_NO_OPERATIONS_ERROR			( -4)
#define EMTP_NOT_SUPPORTED_64_BITS_TRACE_ERROR		( -3)
#define EMTP_PARSE_ERROR				( -2)
#define EMTP_UNKNOWN_TAG_ERROR				( -1)
#define EMTP_END_OF_TRACE				(  0)
#define EMTP_END_OF_TRACE_GARBAGE_FOLLOWS		(  1)
#define EMTP_ALL_OPS_FILLED		       		(  2)
#define EMTP_NEED_MORE_TRACE			 	(  3)
#define EMTP_HEADER_PARSED				(  4)

/* Allocator flags */
#define EMTP_ALLOCATOR_FLAG_HAVE_USED_CARRIERS_INFO	(1 << 0)

/* Block type flags */
/* #define EMTP_BLOCK_TYPE_FLAG_X */


typedef struct {
    usgnd_int_32	major;
    usgnd_int_32	minor;
} emtp_version;

typedef struct {
    emtp_version	parser;
    emtp_version	trace;
} emtp_versions;

typedef struct {
    int			valid;
    usgnd_int_32	flags;
    char *		name;
    struct {
	usgnd_int_16	no_providers;
	usgnd_int_16 *	provider;
    } carrier;
} emtp_allocator;

typedef struct {
    int			valid;
    usgnd_int_32	flags;
    char *		name;
    sgnd_int_32		allocator;
} emtp_block_type;

typedef struct {
    emtp_versions		version;
    int				bits;
    char *			nodename;
    char *			hostname;
    char *			pid;
    struct {
	usgnd_int_32		year;
	usgnd_int_32		month;
	usgnd_int_32		day;
	usgnd_int_32		hour;
	usgnd_int_32		minute;
	usgnd_int_32		second;
	usgnd_int_32		micro_second;
    } start_time;
    usgnd_int_16		segment_ix;
    usgnd_int_16		max_allocator_ix;
    emtp_allocator **		allocator;
    usgnd_int_16		max_block_type_ix;
    emtp_block_type **		block_type;
    int				have_carrier_info;
    int				have_segment_carrier_info;
} emtp_info;

typedef struct emtp_state_ emtp_state;

enum emtp_op_type_ {
    EMTP_UNDEF			= 0,
    EMTP_ALLOC			= 1,
    EMTP_REALLOC		= 2,
    EMTP_FREE			= 3,
    EMTP_CARRIER_ALLOC		= 4,
    EMTP_CARRIER_REALLOC	= 5,
    EMTP_CARRIER_FREE		= 6,
    EMTP_STOP			= 7,
    EMTP_EXIT 			= 8
};

typedef enum emtp_op_type_ emtp_op_type;

typedef struct {
    usgnd_int_16 	type;
    usgnd_int_16	carrier_type;
    usgnd_int_max	new_ptr;
    usgnd_int_max	prev_ptr;
    usgnd_int_max	new_size;
} emtp_block_op;

typedef struct {
    emtp_op_type	type;
    struct {
	usgnd_int_32	secs;
	usgnd_int_32	usecs;
    } time;
    union {
	emtp_block_op	block;
	usgnd_int_32	exit_status;
    } u;
} emtp_operation;

const char *emtp_error_string(int);
int emtp_get_info(emtp_info *ip, size_t *isz, emtp_state *sp);
emtp_state *emtp_state_new(void * (*alloc)(size_t),
			   void * (*realloc)(void *, size_t),
			   void   (*free)(void *));
void emtp_state_destroy(emtp_state *sp);
int emtp_parse(emtp_state *sp,
	       usgnd_int_8 **tracepp, size_t *trace_lenp,
	       emtp_operation *op_start, size_t op_size, size_t *op_lenp);
#endif
