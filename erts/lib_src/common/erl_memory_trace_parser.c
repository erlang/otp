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

#include "erl_memory_trace_parser.h"
#include "erl_memory_trace_protocol.h"
#include <string.h> /* For memcpy */

#ifdef DEBUG
#include <assert.h>
#define ASSERT assert
#define PRINT_ERROR_ORIGIN 1
#if PRINT_ERROR_ORIGIN
#include <stdio.h>
#endif
#define PRINT_PARSED_OP 0
#if PRINT_PARSED_OP
#include <stdio.h>
static void print_op(emtp_operation *op_p);
#endif
static void hexdump(void *start, void *end);
#else
#define PRINT_ERROR_ORIGIN 0
#define PRINT_PARSED_OP 0
#define ASSERT(B)
#endif


#if ERTS_MT_MAJOR_VSN != 2 || ERTS_MT_MINOR_VSN != 0
#error trace version mismatch (expected version 2.0)
/* Make sure that older versions are supported when implementing
   support for newer versions! */
#endif


#if defined(__GNUC__)
#  define EMTP_CAN_INLINE 1
#  define EMTP_INLINE __inline__
#elif defined(__WIN32__)
#  define EMTP_CAN_INLINE 1
#  define EMTP_INLINE __forceinline
#else
#  define EMTP_CAN_INLINE 0
#  define EMTP_INLINE
#endif


#define UI8_SZ				1
#define UI16_SZ				2
#define UI32_SZ				4
#define UI64_SZ				8

#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))
#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))

#define DEFAULT_OVERFLOW_BUF_SZ		128

#define UNKNOWN_BLOCK_TYPE_IX		(-1)
#define UNKNOWN_ALLOCATOR_IX		(-1)

#define INVALID_SIZE			(((sgnd_int_32) 1) << 31)
#define INVALID_RESULT			((int) INVALID_SIZE)

typedef enum {
    EMTP_PROGRESS_PARSE_HDR_VSN,
    EMTP_PROGRESS_PARSE_HDR_PROLOG,
    EMTP_PROGRESS_ALLOC_HDR_INFO,
    EMTP_PROGRESS_PARSE_TAGGED_HDR,
    EMTP_PROGRESS_PARSE_BODY,
    EMTP_PROGRESS_ENDED
} emtp_progress;

struct emtp_state_ {

    /* Trace version */
    emtp_version	version;

    /* Flags */
    usgnd_int_32	flags;

    /* Progress */
    emtp_progress	progress;

    /* Name, host, and pid as strings */
    char 		nodename[256];
    char 		hostname[256];
    char 		pid[256];

    /* Local time on the traced node when the node started */
    struct {
	usgnd_int_32	year;
	usgnd_int_32	month;
	usgnd_int_32	day;
	usgnd_int_32	hour;
	usgnd_int_32	minute;
	usgnd_int_32	second;
	usgnd_int_32	micro_second;
    } start_time;

    /* Function to parse body with */
    int 		(*parse_body_func)(emtp_state *,
					   usgnd_int_8 **,
					   usgnd_int_8 *,
					   emtp_operation **,
					   emtp_operation *,
					   size_t);
    /* Current time elapsed */
    struct {
	usgnd_int_32	secs;
	usgnd_int_32	usecs;
    } time;

    /* */

    int			force_return;

    /* Overflow buffer */
    size_t		overflow_size;
    size_t		overflow_buf_size;
    usgnd_int_8	*	overflow;
    sgnd_int_32		fetch_size;
    int			known_need;

    usgnd_int_16	segment_ix;
    usgnd_int_16	max_allocator_ix;
    emtp_allocator **	allocator;
    usgnd_int_16	max_block_type_ix;
    emtp_block_type **	block_type;
    
    /* Memory allocation functions */
    void * (*alloc)(size_t);
    void * (*realloc)(void *, size_t);
    void   (*free)(void *);

};

static char unknown_allocator[] = "unknown_allocator";
static char unknown_block_type[] = "unknown_block_type";

const char *
emtp_error_string(int res)
{
    switch (res) {
    case EMTP_NO_TRACE_ERROR:
	return "no trace error";
    case EMTP_HEADER_TAG_IN_BODY_ERROR:
	return "header tag in body error";
    case EMTP_BODY_TAG_IN_HEADER_ERROR:
	return "body tag in header error";
    case EMTP_NOT_SUPPORTED_MTRACE_VERSION_ERROR:
	return "not supported mtrace version error";
    case EMTP_NOT_AN_ERL_MTRACE_ERROR:
	return "not an erl mtrace error";
    case EMTP_NO_MEMORY_ERROR:
	return "no memory error";
    case EMTP_BAD_OP_SIZE_ERROR:
	return "bad op size error";
    case EMTP_NO_OPERATIONS_ERROR:
	return "no operations error";
    case EMTP_NOT_SUPPORTED_64_BITS_TRACE_ERROR:
	return "not supported 64 bits trace error";
    case EMTP_PARSE_ERROR:
	return "parse error";
    case EMTP_UNKNOWN_TAG_ERROR:
	return "unknown tag error";
    case EMTP_END_OF_TRACE:
	return "end of trace";
    case EMTP_END_OF_TRACE_GARBAGE_FOLLOWS:
	return "end of trace; garbage follows";
    case EMTP_ALL_OPS_FILLED:
	return "all operations filled";
    case EMTP_NEED_MORE_TRACE:
	return "need more trace";
    case EMTP_HEADER_PARSED:
	return "header parsed";
    default:
	return NULL;
    }

}

int
emtp_get_info(emtp_info *infop, size_t *info_szp, emtp_state *statep)
{
    if (!infop || !info_szp || *info_szp < sizeof(emtp_info))
	return 0;

    infop->version.parser.major		= ERTS_MT_MAJOR_VSN;
    infop->version.parser.minor		= ERTS_MT_MINOR_VSN;

    *info_szp 				= sizeof(emtp_version);

    if (!statep || statep->version.major == 0)
	return 1;

    infop->version.trace.major		= statep->version.major;
    infop->version.trace.minor		= statep->version.minor;

    *info_szp 				= sizeof(emtp_versions);

    if (statep->progress != EMTP_PROGRESS_PARSE_BODY
	&& statep->progress != EMTP_PROGRESS_ENDED)
	return 1;

    infop->bits				= (statep->flags & ERTS_MT_64_BIT_FLAG
					   ? 64
					   : 32);

    infop->nodename			= statep->nodename;
    infop->hostname			= statep->hostname;
    infop->pid				= statep->pid;

    infop->start_time.year		= statep->start_time.year;
    infop->start_time.month		= statep->start_time.month;
    infop->start_time.day		= statep->start_time.day;
    infop->start_time.hour		= statep->start_time.hour;
    infop->start_time.minute		= statep->start_time.minute;
    infop->start_time.second		= statep->start_time.second;
    infop->start_time.micro_second	= statep->start_time.micro_second;

    infop->have_carrier_info		= statep->flags & ERTS_MT_CRR_INFO;
    infop->have_segment_carrier_info	= statep->flags & ERTS_MT_SEG_CRR_INFO;
    infop->segment_ix			= statep->segment_ix;
    infop->max_allocator_ix		= statep->max_allocator_ix;
    infop->allocator			= statep->allocator;
    infop->max_block_type_ix		= statep->max_block_type_ix;
    infop->block_type			= statep->block_type;

    *info_szp 				= sizeof(emtp_info);

    return 1;
}

emtp_state *
emtp_state_new(void * (*alloc)(size_t),
	       void * (*realloc)(void *, size_t),
	       void   (*free)(void *))
{
    emtp_state *statep;

    if (!alloc || !realloc || !free)
	return NULL;

    statep = (emtp_state *) (*alloc)(sizeof(emtp_state));
    if (!statep)
	return NULL;

    statep->version.major			= 0;
    statep->version.minor			= 0;
    statep->flags				= 0;
    statep->progress				= EMTP_PROGRESS_PARSE_HDR_VSN;

    statep->nodename[0]				= '\0';
    statep->hostname[0]				= '\0';
    statep->pid[0]				= '\0';

    statep->start_time.year			= 0;
    statep->start_time.month			= 0;
    statep->start_time.day			= 0;
    statep->start_time.hour			= 0;
    statep->start_time.minute			= 0;
    statep->start_time.second			= 0;
    statep->start_time.micro_second		= 0;

    statep->parse_body_func			= NULL;
    statep->time.secs				= 0;
    statep->time.usecs				= 0;
    statep->force_return			= 0;
    statep->overflow_size			= 0;
    statep->overflow_buf_size			= DEFAULT_OVERFLOW_BUF_SZ;
    statep->overflow				= 
	(usgnd_int_8 *) (*alloc)(DEFAULT_OVERFLOW_BUF_SZ*sizeof(usgnd_int_8));
    statep->fetch_size				= 0;
    statep->known_need				= 0;
    statep->segment_ix				= 0;
    statep->max_allocator_ix			= 0;
    statep->allocator				= NULL;
    statep->max_block_type_ix			= 0;
    statep->block_type				= NULL;
    statep->alloc				= alloc;
    statep->realloc				= realloc;
    statep->free				= free;

    return statep;
}

void
emtp_state_destroy(emtp_state *statep)
{
    void (*freep)(void *);
    int i;

    if (!statep)
	return;

    freep = statep->free;

    if (statep->overflow)
	(*freep)((void *) statep->overflow);

    if (statep->allocator) {
	for (i = -1; i <= statep->max_allocator_ix; i++) {
	    if (statep->allocator[i]) {
		if (statep->allocator[i]->name
		    && statep->allocator[i]->name != unknown_allocator)
		    (*freep)((void *) statep->allocator[i]->name);
		if (statep->allocator[i]->carrier.provider)
		    (*freep)((void *) statep->allocator[i]->carrier.provider);
		(*freep)((void *) statep->allocator[i]);
	    }
	}
	statep->allocator--;
	(*freep)((void *) statep->allocator);
    }

    if (statep->block_type) {
	for (i = -1; i <= statep->max_block_type_ix; i++) {
	    if (statep->block_type[i]) {
		if (statep->block_type[i]->name
		    && statep->block_type[i]->name != unknown_block_type)
		    (*freep)((void *) statep->block_type[i]->name);
		(*freep)((void *) statep->block_type[i]);
	    }
	}
	statep->block_type--;
	(*freep)((void *) statep->block_type);
    }

    (*freep)((void *) statep);
}
		      
/*
 * The following macros are for use in emtp_parse(), parse_vX_body,
 * and parse_header.
 *
 * Note that some of them depend on function local variable names
 * and lables:
 *
 *  Variables:
 *  * result		-> the result to return
 *  * statep		-> pointer to the state
 *
 *  Lables:
 *  * restore_return	-> restore then return result
 */


#define GET_UI8(UI, BP) ((UI) = *((BP)++))
#define SKIP_UI8(BP) ((BP)++)

#define GET_UI16(UI, BP)						\
 do {									\
     (UI) = ((( (usgnd_int_16) (BP)[0]) << 8)				\
	     | ((usgnd_int_16) (BP)[1]));				\
     (BP) += UI16_SZ;							\
} while(0)
#define SKIP_UI16(BP) ((BP) += UI16_SZ)


#define GET_UI32(UI, BP)						\
 do {									\
     (UI) = (((  (usgnd_int_32) (BP)[0]) << 24)				\
	     | (((usgnd_int_32) (BP)[1]) << 16)				\
	     | (((usgnd_int_32) (BP)[2]) << 8)				\
	     | ( (usgnd_int_32) (BP)[3]));				\
     (BP) += UI32_SZ;							\
} while(0)
#define SKIP_UI32(BP) ((BP) += UI32_SZ)

#define GET_UI64(UI, BP)						\
 do {									\
     (UI) = (((  (usgnd_int_64) (BP)[0]) << 56)				\
	     | (((usgnd_int_64) (BP)[1]) << 48)				\
	     | (((usgnd_int_64) (BP)[2]) << 40)				\
	     | (((usgnd_int_64) (BP)[3]) << 32)				\
	     | (((usgnd_int_64) (BP)[4]) << 24)				\
	     | (((usgnd_int_64) (BP)[5]) << 16)				\
	     | (((usgnd_int_64) (BP)[6]) << 8)				\
	     | ( (usgnd_int_64) (BP)[7]));				\
     (BP) += UI64_SZ;							\
} while(0)
#define SKIP_UI64(BP) ((BP) += UI64_SZ)

#define GET_VSZ_UI16(UI, BP, MSB)					\
do {									\
    usgnd_int_16 ui_ = 0;				       		\
    switch ((MSB)) {							\
    case 1: ui_ |= (usgnd_int_16) *((BP)++); ui_ <<= 8;			\
    case 0: ui_ |= (usgnd_int_16) *((BP)++); break;			\
    default: ERROR(EMTP_PARSE_ERROR);					\
    }									\
    (UI) = ui_;								\
} while (0)

#define GET_VSZ_UI32(UI, BP, MSB)					\
do {									\
    usgnd_int_32 ui_ = 0;						\
    switch ((MSB)) {							\
    case 3: ui_ |= (usgnd_int_32) *((BP)++); ui_ <<= 8;			\
    case 2: ui_ |= (usgnd_int_32) *((BP)++); ui_ <<= 8;			\
    case 1: ui_ |= (usgnd_int_32) *((BP)++); ui_ <<= 8;			\
    case 0: ui_ |= (usgnd_int_32) *((BP)++); break;			\
    default: ERROR(EMTP_PARSE_ERROR);					\
    }									\
    (UI) = ui_;								\
} while (0)

#define GET_VSZ_UI64(UI, BP, MSB)					\
do {									\
    usgnd_int_64 ui_ = 0;						\
    switch ((MSB)) {							\
    case 7: ui_ |= (usgnd_int_64) *((BP)++); ui_ <<= 8;			\
    case 6: ui_ |= (usgnd_int_64) *((BP)++); ui_ <<= 8;			\
    case 5: ui_ |= (usgnd_int_64) *((BP)++); ui_ <<= 8;			\
    case 4: ui_ |= (usgnd_int_64) *((BP)++); ui_ <<= 8;			\
    case 3: ui_ |= (usgnd_int_64) *((BP)++); ui_ <<= 8;			\
    case 2: ui_ |= (usgnd_int_64) *((BP)++); ui_ <<= 8;			\
    case 1: ui_ |= (usgnd_int_64) *((BP)++); ui_ <<= 8;			\
    case 0: ui_ |= (usgnd_int_64) *((BP)++); break;			\
    default: ERROR(EMTP_PARSE_ERROR);					\
    }									\
    (UI) = ui_;								\
} while (0)


#if HAVE_INT_64
#define GET_VSZ_UIMAX(UI, BP, MSB)					\
do {									\
    usgnd_int_64 ui64_;							\
    GET_VSZ_UI64(ui64_, (BP), (MSB));					\
    (UI) = (usgnd_int_max) ui64_;					\
} while (0)
#else
#define GET_VSZ_UIMAX(UI, BP, MSB)					\
do {									\
    usgnd_int_32 ui32_;							\
    GET_VSZ_UI32(ui32_, (BP), (MSB));					\
    (UI) = (usgnd_int_max) ui32_;					\
} while (0)
#endif
	


#define INC_TIME(C_SECS, C_USECS, SECS, USECS)				\
do {									\
    if ((USECS) >= 1000000)						\
	ERROR(EMTP_PARSE_ERROR);					\
    (C_SECS)  += (SECS);						\
    (C_USECS) += (USECS);						\
    if ((C_USECS) >= 1000000) {						\
	(C_USECS) -= 1000000;						\
	(C_SECS)++;							\
    }									\
} while (0)

#if PRINT_ERROR_ORIGIN
#include <stdio.h>
#define ERROR(E)							\
do {									\
    result = (E);							\
    fprintf(stderr,"ERROR:%s:%d: result=%d\n",__FILE__,__LINE__,result);\
    statep->force_return = 1; abort();						\
    goto restore_return;						\
} while (0)
#else
#define ERROR(E) do {							\
    result = (E);							\
    statep->force_return = 1;						\
    goto restore_return;						\
} while (0)
#endif

#define NEED(NSZ, TSZ)							\
do {									\
    sgnd_int_32 need_ = (NSZ);						\
    if (need_ > (TSZ)) {						\
	statep->known_need = 1;						\
	statep->fetch_size = need_;					\
	result = EMTP_NEED_MORE_TRACE;					\
	goto restore_return;						\
    }									\
} while (0)

#define NEED_AT_LEAST(NSZ, FSZ, TSZ)					\
do {									\
    sgnd_int_32 need_ = (NSZ);						\
    ASSERT(need_ <= (FSZ));						\
    if (need_ > (TSZ)) {						\
	statep->known_need = 0;						\
	statep->fetch_size = (FSZ);					\
	result = EMTP_NEED_MORE_TRACE;					\
	goto restore_return;						\
    }									\
} while (0)


#define SECS_PER_DAY (60*60*24)
#define IS_LEAP_YEAR(X) (((X) % 4 == 0 && (X) % 100 != 0) || (X) % 400 == 0)

static void
set_start_time(emtp_state *state,
	       usgnd_int_32 giga_seconds,
	       usgnd_int_32 seconds,
	       usgnd_int_32 micro_seconds)
{
    /* Input is elapsed time since 1970-01-01 00:00.000000 (UTC) */

    usgnd_int_32 year, days_of_this_year, days, secs, month;
    usgnd_int_32 days_of_month[] = {0,31,28,31,30,31,30,31,31,30,31,30,31};

    days = 1000000000 / SECS_PER_DAY;
    secs = 1000000000 % SECS_PER_DAY;
    days *= giga_seconds;
    secs *= giga_seconds;
    secs += seconds;
    days += secs / SECS_PER_DAY;
    secs %= SECS_PER_DAY;
    days++;

    year = 1969;
    days_of_this_year = 0;
    while (days > days_of_this_year) {
	days -= days_of_this_year;
	year++;
	days_of_this_year = 365 + (IS_LEAP_YEAR(year) ? 1 : 0);
    }

    for (month = 1; month <= 12; month++) {
	usgnd_int_32 days_of_this_month = days_of_month[month];
	if (month == 2 && IS_LEAP_YEAR(year))
	    days_of_this_month++;
	if (days <= days_of_this_month)
	    break;
	days -= days_of_this_month;
    }

    state->start_time.year = year;
    state->start_time.month = month;
    state->start_time.day = days;
    state->start_time.hour = secs / (60*60);
    secs %= 60*60;
    state->start_time.minute = secs / 60;
    state->start_time.second = secs % 60;
    state->start_time.micro_second = micro_seconds;
}

static int
parse_v1_body(emtp_state *statep,
	      usgnd_int_8 **tracepp, usgnd_int_8 *trace_endp,
	      emtp_operation **op_pp, emtp_operation *op_endp, size_t op_size)
{
    /* "cache" some frequently used values */
    register usgnd_int_8 *c_p		= *tracepp;
    register emtp_operation *op_p	= *op_pp;
    register usgnd_int_32 current_secs	= statep->time.secs;
    register usgnd_int_32 current_usecs	= statep->time.usecs;

    sgnd_int_32 trace_size		= trace_endp - c_p;
    usgnd_int_8 *tracep			= c_p;
    int result				= 0;

    usgnd_int_16 max_block_type		= statep->max_block_type_ix;

    while (trace_size >= UI16_SZ) {
	usgnd_int_16 ehdr, tag;
	unsigned time_inc_msb;

	GET_UI16(ehdr, c_p);
	tag = ehdr & ERTS_MT_TAG_EHDR_FLD_MSK;
	switch (tag) {
	case ERTS_MT_V1_ALLOC_TAG:

	    op_p->type = EMTP_ALLOC;

	alloc_common: {
		usgnd_int_16 block_type;
		unsigned block_type_msb, new_ptr_msb, new_size_msb;

		ehdr >>= ERTS_MT_TAG_EHDR_FLD_SZ;
		block_type_msb = ehdr & ERTS_MT_UI16_MSB_EHDR_FLD_MSK;
		ehdr >>= ERTS_MT_UI16_MSB_EHDR_FLD_SZ;
		new_ptr_msb = ehdr & ERTS_MT_UI_MSB_EHDR_FLD_MSK;
		ehdr >>= ERTS_MT_UI_MSB_EHDR_FLD_SZ;
		new_size_msb = ehdr & ERTS_MT_UI_MSB_EHDR_FLD_MSK;
		ehdr >>= ERTS_MT_UI_MSB_EHDR_FLD_SZ;
		time_inc_msb = ehdr & ERTS_MT_UI32_MSB_EHDR_FLD_MSK;

		NEED(UI16_SZ
		     + 4
		     + block_type_msb
		     + new_ptr_msb
		     + new_size_msb
		     + time_inc_msb,
		     trace_size);

		GET_VSZ_UI16(block_type, c_p, block_type_msb);
		if (block_type > max_block_type)
		    ERROR(EMTP_PARSE_ERROR);
		op_p->u.block.type = (int) block_type;

		GET_VSZ_UIMAX(op_p->u.block.new_ptr,  c_p, new_ptr_msb);
		GET_VSZ_UIMAX(op_p->u.block.new_size, c_p, new_size_msb);

		op_p->u.block.prev_ptr = 0;
	    }

	read_time_inc: {
	    usgnd_int_32 secs, usecs, time_inc;

	    GET_VSZ_UI32(time_inc, c_p, time_inc_msb);

	    secs  = ((time_inc >> ERTS_MT_TIME_INC_SECS_SHIFT)
		     & ERTS_MT_TIME_INC_SECS_MASK);
	    usecs = ((time_inc >> ERTS_MT_TIME_INC_USECS_SHIFT)
		     & ERTS_MT_TIME_INC_USECS_MASK);

	    INC_TIME(current_secs, current_usecs, secs, usecs);

	    op_p->time.secs  = current_secs;
	    op_p->time.usecs = current_usecs;

#if PRINT_PARSED_OP
	    print_op(op_p);
#endif

	    op_p = (emtp_operation *) (((char *) op_p) + op_size);
	    break;
	}

	case ERTS_MT_V1_REALLOC_NPB_TAG:
	    op_p->type = EMTP_REALLOC;
	    goto alloc_common;

	case ERTS_MT_V1_REALLOC_MV_TAG: {
	    unsigned new_ptr_msb, prev_ptr_msb, new_size_msb;

	    op_p->type = EMTP_REALLOC;

	    ehdr >>= ERTS_MT_TAG_EHDR_FLD_SZ;
	    new_ptr_msb = ehdr & ERTS_MT_UI_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI_MSB_EHDR_FLD_SZ;
	    prev_ptr_msb = ehdr & ERTS_MT_UI_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI_MSB_EHDR_FLD_SZ;
	    new_size_msb = ehdr & ERTS_MT_UI_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI_MSB_EHDR_FLD_SZ;
	    time_inc_msb = ehdr & ERTS_MT_UI32_MSB_EHDR_FLD_MSK;

	    NEED(UI16_SZ
		 + 4
		 + new_ptr_msb
		 + prev_ptr_msb
		 + new_size_msb
		 + time_inc_msb,
		 trace_size);

	    GET_VSZ_UIMAX(op_p->u.block.new_ptr,  c_p, new_ptr_msb);
	    GET_VSZ_UIMAX(op_p->u.block.prev_ptr, c_p, prev_ptr_msb);
	    GET_VSZ_UIMAX(op_p->u.block.new_size, c_p, new_size_msb);

	    op_p->u.block.type = UNKNOWN_BLOCK_TYPE_IX;
	    goto read_time_inc;
	}

	case ERTS_MT_V1_REALLOC_NMV_TAG: {
	    usgnd_int_max new_ptr;
	    unsigned new_ptr_msb, new_size_msb;

	    op_p->type = EMTP_REALLOC;

	    ehdr >>= ERTS_MT_TAG_EHDR_FLD_SZ;
	    new_ptr_msb = ehdr & ERTS_MT_UI_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI_MSB_EHDR_FLD_SZ;
	    new_size_msb = ehdr & ERTS_MT_UI_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI_MSB_EHDR_FLD_SZ;
	    time_inc_msb = ehdr & ERTS_MT_UI32_MSB_EHDR_FLD_MSK;

	    NEED(UI16_SZ
		 + 3
		 + new_ptr_msb
		 + new_size_msb
		 + time_inc_msb,
		 trace_size);

	    GET_VSZ_UIMAX(new_ptr,                c_p, new_ptr_msb);
	    GET_VSZ_UIMAX(op_p->u.block.new_size, c_p, new_size_msb);

	    op_p->u.block.new_ptr   = new_ptr;
	    op_p->u.block.prev_ptr  = new_ptr;

	    op_p->u.block.type          = UNKNOWN_BLOCK_TYPE_IX;
	    goto read_time_inc;
	}

	case ERTS_MT_V1_FREE_TAG: {
	    unsigned prev_ptr_msb;

	    op_p->type = EMTP_FREE;

	    ehdr >>= ERTS_MT_TAG_EHDR_FLD_SZ;
	    prev_ptr_msb = ehdr & ERTS_MT_UI_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI_MSB_EHDR_FLD_SZ;
	    time_inc_msb = ehdr & ERTS_MT_UI32_MSB_EHDR_FLD_MSK;

	    NEED(UI16_SZ
		 + 2
		 + prev_ptr_msb
		 + time_inc_msb,
		 trace_size);

	    GET_VSZ_UIMAX(op_p->u.block.prev_ptr, c_p, prev_ptr_msb);

	    op_p->u.block.new_ptr  = 0;
	    op_p->u.block.new_size = 0;

	    op_p->u.block.type = UNKNOWN_BLOCK_TYPE_IX;
	    goto read_time_inc;
	}

	case ERTS_MT_V1_TIME_INC_TAG: {
	    unsigned secs_msb, usecs_msb;
	    usgnd_int_32 secs, usecs;

	    ehdr >>= ERTS_MT_TAG_EHDR_FLD_SZ;

	    secs_msb = ehdr & ERTS_MT_UI32_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI32_MSB_EHDR_FLD_SZ;

	    usecs_msb = ehdr & ERTS_MT_UI32_MSB_EHDR_FLD_MSK;

	    NEED(UI16_SZ + 2 + secs_msb + usecs_msb, trace_size);

	    GET_VSZ_UI32(secs,  c_p, secs_msb);
	    GET_VSZ_UI32(usecs, c_p, usecs_msb);

	    INC_TIME(current_secs, current_usecs, secs, usecs);

	    break;
	}

	case ERTS_MT_V1_STOP_TAG:

	    op_p->type = EMTP_STOP;

	    ehdr >>= ERTS_MT_TAG_EHDR_FLD_SZ;

	    time_inc_msb = ehdr & ERTS_MT_UI32_MSB_EHDR_FLD_MSK;

	    NEED(UI16_SZ + 1 + time_inc_msb, trace_size);

	    goto read_ending_time_inc;

	case ERTS_MT_V1_EXIT_TAG: {
	    unsigned exit_status_msb;

	    op_p->type = EMTP_EXIT;

	    ehdr >>= ERTS_MT_TAG_EHDR_FLD_SZ;
	    exit_status_msb = ehdr & ERTS_MT_UI32_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI32_MSB_EHDR_FLD_SZ;
	    time_inc_msb = ehdr & ERTS_MT_UI32_MSB_EHDR_FLD_MSK;

	    NEED(UI16_SZ + 2 + exit_status_msb + time_inc_msb,
		 trace_size);

	    GET_VSZ_UI32(op_p->u.exit_status, c_p, exit_status_msb);

	read_ending_time_inc: {
		usgnd_int_32 secs, usecs, time_inc;

		GET_VSZ_UI32(time_inc, c_p, time_inc_msb);

		secs  = ((time_inc >> ERTS_MT_TIME_INC_SECS_SHIFT)
			 & ERTS_MT_TIME_INC_SECS_MASK);
		usecs = ((time_inc >> ERTS_MT_TIME_INC_USECS_SHIFT)
			 & ERTS_MT_TIME_INC_USECS_MASK);

		INC_TIME(current_secs, current_usecs, secs, usecs);

		op_p->time.secs  = current_secs;
		op_p->time.usecs = current_usecs;

#if PRINT_PARSED_OP
		print_op(op_p);
#endif

		op_p = (emtp_operation *) (((char *) op_p) + op_size);
		statep->force_return = 1;
		statep->progress = EMTP_PROGRESS_ENDED;

		tracep = c_p;
		trace_size = trace_endp - tracep;
		result = (trace_size
			  ? EMTP_END_OF_TRACE_GARBAGE_FOLLOWS
			  : EMTP_END_OF_TRACE);
		goto restore_return;
	    }
	}

	case ERTS_MT_V1_ALLOCATOR_TAG:
	case ERTS_MT_V1_BLOCK_TYPE_TAG:

#ifdef DEBUG
	    hexdump(tracep, trace_endp);
#endif
	    ERROR(EMTP_HEADER_TAG_IN_BODY_ERROR);

	default:

#ifdef DEBUG
	    hexdump(tracep, trace_endp);
#endif
	    ERROR(EMTP_UNKNOWN_TAG_ERROR);
	}

	tracep = c_p;
	trace_size = trace_endp - tracep;

	if (op_p >= op_endp) {
	    statep->force_return = 1;
	    result = EMTP_ALL_OPS_FILLED;
	    goto restore_return;
	}
    }

    statep->known_need = 0;
    statep->fetch_size = ERTS_MT_MAX_V1_BODY_ENTRY_SIZE;

    result = EMTP_NEED_MORE_TRACE;

 restore_return:
    *tracepp		= tracep;
    *op_pp		= op_p;
    statep->time.secs	= current_secs;
    statep->time.usecs	= current_usecs;

    return result;
}

#define GET_ALLOC_MSBS(EHDR, BT, NP, NS, TI)		\
do {							\
    (BT) = (EHDR) & ERTS_MT_UI16_MSB_EHDR_FLD_MSK;	\
    (EHDR) >>= ERTS_MT_UI16_MSB_EHDR_FLD_SZ;		\
    (NP) = (EHDR) & ERTS_MT_UI_MSB_EHDR_FLD_MSK;	\
    (EHDR) >>= ERTS_MT_UI_MSB_EHDR_FLD_SZ;		\
    (NS) = (EHDR) & ERTS_MT_UI_MSB_EHDR_FLD_MSK;	\
    (EHDR) >>= ERTS_MT_UI_MSB_EHDR_FLD_SZ;		\
    (TI) = (EHDR) & ERTS_MT_UI32_MSB_EHDR_FLD_MSK;	\
} while (0)


static EMTP_INLINE int
parse_v2_body(emtp_state *statep,
	      usgnd_int_8 **tracepp, usgnd_int_8 *trace_endp,
	      emtp_operation **op_pp, emtp_operation *op_endp, size_t op_size)
{
    /* "cache" some frequently used values */
    register usgnd_int_8 *c_p		= *tracepp;
    register emtp_operation *op_p	= *op_pp;
    register usgnd_int_32 current_secs	= statep->time.secs;
    register usgnd_int_32 current_usecs	= statep->time.usecs;

    sgnd_int_32 trace_size		= trace_endp - c_p;
    usgnd_int_8 *tracep			= c_p;
    int result				= 0;

    while (trace_size >= UI8_SZ + UI16_SZ) {
	usgnd_int_8 tag;
	usgnd_int_16 ehdr;
	unsigned time_inc_msb;

	tag = *(c_p++);

	GET_UI16(ehdr, c_p);

	switch (tag) {

	case ERTS_MT_CRR_ALLOC_BDY_TAG: {
	    usgnd_int_16 type;
	    unsigned carrier_bytes, carrier_type_msb, block_type_msb,
		new_ptr_msb, new_size_msb;

	    op_p->type = EMTP_CARRIER_ALLOC;

	    carrier_type_msb = ehdr & ERTS_MT_UI16_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI16_MSB_EHDR_FLD_SZ;

	    if (trace_size < ERTS_MT_MAX_CRR_ALLOC_SIZE)
		NEED_AT_LEAST(UI8_SZ + UI16_SZ + 1 + carrier_type_msb,
			      ERTS_MT_MAX_CRR_ALLOC_SIZE,
			      trace_size);

	    GET_VSZ_UI16(type, c_p, carrier_type_msb);
	    op_p->u.block.carrier_type = (int) type;

	    carrier_bytes = carrier_type_msb + 1;
	    goto alloc_common;

	case ERTS_MT_ALLOC_BDY_TAG:

	    op_p->type = EMTP_ALLOC;
	    carrier_bytes = 0;

	alloc_common:
	    block_type_msb = ehdr & ERTS_MT_UI16_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI16_MSB_EHDR_FLD_SZ;
	    new_ptr_msb = ehdr & ERTS_MT_UI_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI_MSB_EHDR_FLD_SZ;
	    new_size_msb = ehdr & ERTS_MT_UI_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI_MSB_EHDR_FLD_SZ;
	    time_inc_msb = ehdr & ERTS_MT_UI32_MSB_EHDR_FLD_MSK;

	    if (trace_size < ERTS_MT_MAX_CRR_ALLOC_SIZE)
		NEED(UI8_SZ
		     + UI16_SZ
		     + 4
		     + carrier_bytes
		     + block_type_msb
		     + new_ptr_msb
		     + new_size_msb
		     + time_inc_msb,
		     trace_size);

	    GET_VSZ_UI16(type, c_p, block_type_msb);
	    op_p->u.block.type = (int) type;

	    GET_VSZ_UIMAX(op_p->u.block.new_ptr,  c_p, new_ptr_msb);
	    GET_VSZ_UIMAX(op_p->u.block.new_size, c_p, new_size_msb);

	    op_p->u.block.prev_ptr = 0;
	}

	read_time_inc: {
	    usgnd_int_32 secs, usecs, time_inc;

	    GET_VSZ_UI32(time_inc, c_p, time_inc_msb);

	    secs  = ((time_inc >> ERTS_MT_TIME_INC_SECS_SHIFT)
		     & ERTS_MT_TIME_INC_SECS_MASK);
	    usecs = ((time_inc >> ERTS_MT_TIME_INC_USECS_SHIFT)
		     & ERTS_MT_TIME_INC_USECS_MASK);

	    INC_TIME(current_secs, current_usecs, secs, usecs);

	    op_p->time.secs  = current_secs;
	    op_p->time.usecs = current_usecs;

#if PRINT_PARSED_OP
	    print_op(op_p);
#endif

	    op_p = (emtp_operation *) (((char *) op_p) + op_size);
	    break;
	}

	case ERTS_MT_CRR_REALLOC_BDY_TAG: {
	    usgnd_int_16 type;
	    unsigned carrier_bytes, carrier_type_msb, block_type_msb,
		new_ptr_msb, prev_ptr_msb, new_size_msb;

	    op_p->type = EMTP_CARRIER_REALLOC;

	    carrier_type_msb = ehdr & ERTS_MT_UI16_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI16_MSB_EHDR_FLD_SZ;

	    if (trace_size < ERTS_MT_MAX_CRR_REALLOC_SIZE)
		NEED_AT_LEAST(UI8_SZ + UI16_SZ + 1 + carrier_type_msb,
			      ERTS_MT_MAX_CRR_REALLOC_SIZE,
			      trace_size);

	    GET_VSZ_UI16(type, c_p, carrier_type_msb);
	    op_p->u.block.carrier_type = (int) type;

	    carrier_bytes = carrier_type_msb + 1;
	    goto realloc_common;

	case ERTS_MT_REALLOC_BDY_TAG:

	    op_p->type = EMTP_REALLOC;
	    carrier_bytes = 0;

	realloc_common:

	    block_type_msb = ehdr & ERTS_MT_UI16_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI16_MSB_EHDR_FLD_SZ;
	    new_ptr_msb = ehdr & ERTS_MT_UI_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI_MSB_EHDR_FLD_SZ;
	    prev_ptr_msb = ehdr & ERTS_MT_UI_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI_MSB_EHDR_FLD_SZ;
	    new_size_msb = ehdr & ERTS_MT_UI_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI_MSB_EHDR_FLD_SZ;
	    time_inc_msb = ehdr & ERTS_MT_UI32_MSB_EHDR_FLD_MSK;

	    if (trace_size < ERTS_MT_MAX_CRR_REALLOC_SIZE)
		NEED(UI8_SZ
		     + UI16_SZ
		     + 5
		     + carrier_bytes
		     + block_type_msb
		     + new_ptr_msb
		     + prev_ptr_msb
		     + new_size_msb
		     + time_inc_msb,
		     trace_size);

	    GET_VSZ_UI16(op_p->u.block.type,   c_p, block_type_msb);
	    GET_VSZ_UIMAX(op_p->u.block.new_ptr,  c_p, new_ptr_msb);
	    GET_VSZ_UIMAX(op_p->u.block.prev_ptr, c_p, prev_ptr_msb);
	    GET_VSZ_UIMAX(op_p->u.block.new_size, c_p, new_size_msb);

	    goto read_time_inc;
	}

	case ERTS_MT_CRR_FREE_BDY_TAG: {
	    usgnd_int_16 type;
	    unsigned carrier_bytes, carrier_type_msb, block_type_msb,
		prev_ptr_msb;

	    op_p->type = EMTP_CARRIER_FREE;

	    carrier_type_msb = ehdr & ERTS_MT_UI16_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI16_MSB_EHDR_FLD_SZ;

	    if (trace_size < ERTS_MT_MAX_CRR_FREE_SIZE)
		NEED_AT_LEAST(UI8_SZ + UI16_SZ + 1 + carrier_type_msb,
			      ERTS_MT_MAX_CRR_FREE_SIZE,
			      trace_size);

	    GET_VSZ_UI16(type, c_p, carrier_type_msb);
	    op_p->u.block.carrier_type = (int) type;

	    carrier_bytes = carrier_type_msb + 1;
	    goto free_common;

	case ERTS_MT_FREE_BDY_TAG:

	    op_p->type = EMTP_FREE;
	    carrier_bytes = 0;

	free_common:

	    block_type_msb = ehdr & ERTS_MT_UI16_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI16_MSB_EHDR_FLD_SZ;
	    prev_ptr_msb = ehdr & ERTS_MT_UI_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI_MSB_EHDR_FLD_SZ;
	    time_inc_msb = ehdr & ERTS_MT_UI32_MSB_EHDR_FLD_MSK;

	    if (trace_size < ERTS_MT_MAX_CRR_FREE_SIZE)
		NEED(UI8_SZ
		     + UI16_SZ
		     + 3
		     + carrier_bytes
		     + block_type_msb
		     + prev_ptr_msb
		     + time_inc_msb,
		     trace_size);

	    GET_VSZ_UI16(op_p->u.block.type,      c_p, block_type_msb);
	    GET_VSZ_UIMAX(op_p->u.block.prev_ptr, c_p, prev_ptr_msb);

	    op_p->u.block.new_ptr  = 0;
	    op_p->u.block.new_size = 0;

	    goto read_time_inc;
	}

	case ERTS_MT_TIME_INC_BDY_TAG: {
	    unsigned secs_msb, usecs_msb;
	    usgnd_int_32 secs, usecs;

	    secs_msb = ehdr & ERTS_MT_UI32_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI32_MSB_EHDR_FLD_SZ;
	    usecs_msb = ehdr & ERTS_MT_UI32_MSB_EHDR_FLD_MSK;

	    NEED(UI8_SZ + UI16_SZ + 2 + secs_msb + usecs_msb, trace_size);

	    GET_VSZ_UI32(secs,  c_p, secs_msb);
	    GET_VSZ_UI32(usecs, c_p, usecs_msb);

	    INC_TIME(current_secs, current_usecs, secs, usecs);

	    break;
	}

	case ERTS_MT_STOP_BDY_TAG:

	    op_p->type = EMTP_STOP;

	    time_inc_msb = ehdr & ERTS_MT_UI32_MSB_EHDR_FLD_MSK;

	    NEED(UI16_SZ + 1 + time_inc_msb, trace_size);

	    goto read_ending_time_inc;

	case ERTS_MT_EXIT_BDY_TAG: {
	    unsigned exit_status_msb;

	    op_p->type = EMTP_EXIT;

	    exit_status_msb = ehdr & ERTS_MT_UI32_MSB_EHDR_FLD_MSK;
	    ehdr >>= ERTS_MT_UI32_MSB_EHDR_FLD_SZ;
	    time_inc_msb = ehdr & ERTS_MT_UI32_MSB_EHDR_FLD_MSK;

	    NEED(UI16_SZ + 2 + exit_status_msb + time_inc_msb, trace_size);

	    GET_VSZ_UI32(op_p->u.exit_status, c_p, exit_status_msb);

	read_ending_time_inc: {
		usgnd_int_32 secs, usecs, time_inc;

		GET_VSZ_UI32(time_inc, c_p, time_inc_msb);

		secs  = ((time_inc >> ERTS_MT_TIME_INC_SECS_SHIFT)
			 & ERTS_MT_TIME_INC_SECS_MASK);
		usecs = ((time_inc >> ERTS_MT_TIME_INC_USECS_SHIFT)
			 & ERTS_MT_TIME_INC_USECS_MASK);

		INC_TIME(current_secs, current_usecs, secs, usecs);

		op_p->time.secs  = current_secs;
		op_p->time.usecs = current_usecs;

#if PRINT_PARSED_OP
		print_op(op_p);
#endif

		op_p = (emtp_operation *) (((char *) op_p) + op_size);
		statep->force_return = 1;
		statep->progress = EMTP_PROGRESS_ENDED;

		tracep = c_p;
		trace_size = trace_endp - tracep;
		result = (trace_size
			  ? EMTP_END_OF_TRACE_GARBAGE_FOLLOWS
			  : EMTP_END_OF_TRACE);
		goto restore_return;
	    }
	}

	case ERTS_MT_X_BDY_TAG: {
	    /* X for extension
	     * ehdr contains total size of entry
	     *
	     * Entry should at least consist of tag (1 byte),
	     * total size (2 bytes) and subtag (1 byte).
	     */
	    if (ehdr < UI8_SZ + UI16_SZ + UI8_SZ)
		ERROR(EMTP_PARSE_ERROR);
	    NEED(ehdr, trace_size);
	    c_p = tracep + ehdr; /* No subtags known yet skip entry... */
	    break;
	}

	default:
#ifdef DEBUG
	    hexdump(c_p-2, trace_endp);
#endif
	    ERROR(EMTP_UNKNOWN_TAG_ERROR);
	}

	tracep = c_p;
	trace_size = trace_endp - tracep;

	if (op_p >= op_endp) {
	    statep->force_return = 1;
	    result = EMTP_ALL_OPS_FILLED;
	    goto restore_return;
	}
    }

    statep->known_need = 0;
    statep->fetch_size = ERTS_MT_MAX_BODY_ENTRY_SIZE;

    result = EMTP_NEED_MORE_TRACE;

 restore_return:
    *tracepp		= tracep;
    *op_pp		= op_p;
    statep->time.secs	= current_secs;
    statep->time.usecs	= current_usecs;

    return result;
}

static void
remove_unused_allocators(emtp_state *statep)
{
    emtp_allocator *allctr;
    sgnd_int_32 i, j, k;
    for (i = -1; i <= statep->max_block_type_ix; i++) {
	if (statep->block_type[i]->valid) {
	    allctr = statep->allocator[statep->block_type[i]->allocator];
	    if (allctr->name != unknown_allocator)
		allctr->valid = 1;
	}
    }
    for (i = -1; i <= statep->max_allocator_ix; i++) {
	allctr = statep->allocator[i];
	if (allctr->valid && allctr->carrier.provider) {
	    for (j = 0; j < allctr->carrier.no_providers; j++) {
		k = allctr->carrier.provider[j];
		if (statep->allocator[k]->name != unknown_allocator)
		    statep->allocator[k]->valid = 1;
	    }
	}
    }
    for (i = -1; i <= statep->max_allocator_ix; i++) {
	allctr = statep->allocator[i];
	if (!allctr->valid) {
	    allctr->flags = 0;
	    if (allctr->name != unknown_allocator) {
		(*statep->free)((void *) allctr->name);
		allctr->name = unknown_allocator;
	    }
	    allctr->carrier.no_providers = 0;
	    if (allctr->carrier.provider) {
		(*statep->free)((void *) allctr->carrier.provider);
	    }
	}
    }
}

static int
parse_header(emtp_state *statep,
	     usgnd_int_8 **tracepp, usgnd_int_8 *trace_endp)
{
    sgnd_int_32 trace_size;
    usgnd_int_8 *tracep;
    int i, result;

    tracep = *tracepp;

    switch (statep->progress) {
    case EMTP_PROGRESS_PARSE_HDR_VSN: {
	usgnd_int_32 start_word;

	trace_size = trace_endp - tracep;
	NEED(3*UI32_SZ, trace_size);

	GET_UI32(start_word, tracep);
	if (start_word != ERTS_MT_START_WORD)
	    return EMTP_NOT_AN_ERL_MTRACE_ERROR;

	GET_UI32(statep->version.major, tracep);
	GET_UI32(statep->version.minor, tracep);

	statep->progress = EMTP_PROGRESS_PARSE_HDR_PROLOG;
    }
    case EMTP_PROGRESS_PARSE_HDR_PROLOG:

	trace_size = trace_endp - tracep;

	switch (statep->version.major) {
	case 1: {
	    NEED(2*UI32_SZ + 2*UI16_SZ, trace_size);

	    GET_UI32(statep->flags, tracep);
	    SKIP_UI32(tracep); /* ignore this; may contain garbage! */
	    GET_UI16(statep->max_allocator_ix, tracep);
	    GET_UI16(statep->max_block_type_ix, tracep);

	    statep->parse_body_func = parse_v1_body;

	    break;
	}
	case 2: {
	    usgnd_int_32 giga_seconds;
	    usgnd_int_32 seconds;
	    usgnd_int_32 micro_seconds;
	    usgnd_int_8 len;
	    usgnd_int_8 *hdr_prolog_start;
	    usgnd_int_32 hdr_prolog_sz;
	    NEED(UI32_SZ, trace_size);
	    hdr_prolog_start = tracep;
	    GET_UI32(hdr_prolog_sz, tracep);
	    NEED(hdr_prolog_sz - UI32_SZ, trace_size);

	    GET_UI32(statep->flags, tracep);
	    GET_UI16(statep->segment_ix, tracep);
	    GET_UI16(statep->max_allocator_ix, tracep);
	    GET_UI16(statep->max_block_type_ix, tracep);

	    GET_UI32(giga_seconds, tracep);
	    GET_UI32(seconds, tracep);
	    GET_UI32(micro_seconds, tracep);

	    set_start_time(statep, giga_seconds, seconds, micro_seconds);

	    GET_UI8(len, tracep);
	    memcpy((void *) statep->nodename, (void *) tracep, (size_t) len);
	    statep->nodename[len] = '\0';
	    tracep += len;

	    GET_UI8(len, tracep);
	    memcpy((void *) statep->hostname, (void *) tracep, (size_t) len);
	    statep->hostname[len] = '\0';
	    tracep += len;

	    GET_UI8(len, tracep);
	    memcpy((void *) statep->pid, (void *) tracep, (size_t) len);
	    statep->pid[len] = '\0';
	    tracep += len;



	    /* Skip things in header prolog we dont know about */
	    tracep = hdr_prolog_start + hdr_prolog_sz;

#if EMTP_CAN_INLINE
	    statep->parse_body_func = NULL;
#else
	    statep->parse_body_func = parse_v2_body;
#endif

	    break;
	}
	default:
	    return EMTP_NOT_SUPPORTED_MTRACE_VERSION_ERROR;
	}

	statep->progress = EMTP_PROGRESS_ALLOC_HDR_INFO;

    case EMTP_PROGRESS_ALLOC_HDR_INFO:

	/* Allocator info */
	if (!statep->allocator) {
	    statep->allocator = (emtp_allocator **)
		(*statep->alloc)((statep->max_allocator_ix + 2)
				 * sizeof(emtp_allocator *));
	    if (!statep->allocator)
		ERROR(EMTP_NO_MEMORY_ERROR);
	    statep->allocator++;
	    for (i = -1; i <= statep->max_allocator_ix; i++)
		statep->allocator[i] = NULL;
	    for (i = -1; i <= statep->max_allocator_ix; i++) {
		statep->allocator[i] = (emtp_allocator *)
		    (*statep->alloc)(sizeof(emtp_allocator));
		if (!statep->allocator[i])
		    ERROR(EMTP_NO_MEMORY_ERROR);
		statep->allocator[i]->valid = 0; 
		statep->allocator[i]->flags = 0; 
		statep->allocator[i]->name = unknown_allocator;
		statep->allocator[i]->carrier.no_providers = 0;
		statep->allocator[i]->carrier.provider = NULL;
	    }
		    
	}

	/* Block type info */
	if (!statep->block_type) {
	    statep->block_type = (emtp_block_type **)
		(*statep->alloc)((statep->max_block_type_ix + 2)
				 * sizeof(emtp_block_type *));
	    if (!statep->block_type)
		ERROR(EMTP_NO_MEMORY_ERROR);
	    statep->block_type++;
	    for (i = -1; i <= statep->max_block_type_ix; i++)
		statep->block_type[i] = NULL;
	    for (i = -1; i <= statep->max_block_type_ix; i++) {
		statep->block_type[i] = (emtp_block_type *)
		    (*statep->alloc)(sizeof(emtp_block_type));
		if (!statep->block_type[i])
		    ERROR(EMTP_NO_MEMORY_ERROR);
		statep->block_type[i]->valid = 0; 
		statep->block_type[i]->flags = 0; 
		statep->block_type[i]->name = unknown_block_type;
		statep->block_type[i]->allocator = UNKNOWN_ALLOCATOR_IX;
	    }
		    
	}

	statep->progress = EMTP_PROGRESS_PARSE_TAGGED_HDR;

    case EMTP_PROGRESS_PARSE_TAGGED_HDR: {
	usgnd_int_8 *c_p = tracep;
	trace_size = trace_endp - tracep;

	switch (statep->version.major) {
	case 1: /* Version 1.X ---------------------------------------------- */

	    while (trace_size >= UI16_SZ) {
		size_t str_len;
		usgnd_int_16 ehdr;

		GET_UI16(ehdr, c_p);

		switch (ehdr & ERTS_MT_TAG_EHDR_FLD_MSK) {
		case ERTS_MT_V1_ALLOCATOR_TAG: {
		    usgnd_int_16 a_ix;

		    NEED_AT_LEAST(2*UI16_SZ + UI8_SZ,
				  ERTS_MT_MAX_HEADER_ENTRY_SIZE,
				  trace_size);

		    GET_UI16(a_ix, c_p);
		    if (a_ix > statep->max_allocator_ix)
			ERROR(EMTP_PARSE_ERROR);

		    GET_UI8(str_len, c_p);

		    NEED(2*UI16_SZ + UI8_SZ + str_len, trace_size);

		    statep->allocator[a_ix]->name
			= (char *) (*statep->alloc)(str_len + 1);
		    if (!statep->allocator[a_ix]->name)
			ERROR(EMTP_NO_MEMORY_ERROR);

		    memcpy((void *) statep->allocator[a_ix]->name,
			   (void *) c_p,
			   str_len);
		    c_p += str_len;

		    statep->allocator[a_ix]->name[str_len] = '\0';
		    break;
		}
		case ERTS_MT_V1_BLOCK_TYPE_TAG: {
		    usgnd_int_16 bt_ix, a_ix;

		    NEED_AT_LEAST(2*UI16_SZ + UI8_SZ,
				  ERTS_MT_MAX_HEADER_ENTRY_SIZE,
				  trace_size);

		    GET_UI16(bt_ix, c_p);
		    if (bt_ix > statep->max_block_type_ix)
			ERROR(EMTP_PARSE_ERROR);

		    GET_UI8(str_len, c_p);

		    NEED(2*UI16_SZ + UI8_SZ + str_len + UI16_SZ, trace_size);

		    statep->block_type[bt_ix]->name
			= (char *) (*statep->alloc)(str_len + 1);

		    if (!statep->block_type[bt_ix]->name)
			ERROR(EMTP_NO_MEMORY_ERROR);

		    memcpy((void *) statep->block_type[bt_ix]->name,
			   (void *) c_p,
			   str_len);
		    c_p += str_len;

		    statep->block_type[bt_ix]->name[str_len] = '\0';

		    GET_UI16(a_ix, c_p);

		    if (a_ix > statep->max_allocator_ix)
			ERROR(EMTP_PARSE_ERROR);

		    statep->block_type[bt_ix]->allocator = (sgnd_int_32) a_ix;
		    statep->block_type[bt_ix]->valid = 1;
		    break;
		}

		case ERTS_MT_V1_ALLOC_TAG:
		case ERTS_MT_V1_REALLOC_NPB_TAG:
		case ERTS_MT_V1_REALLOC_MV_TAG:
		case ERTS_MT_V1_REALLOC_NMV_TAG:
		case ERTS_MT_V1_FREE_TAG:
		case ERTS_MT_V1_TIME_INC_TAG:
		case ERTS_MT_V1_STOP_TAG:
		case ERTS_MT_V1_EXIT_TAG:
		    remove_unused_allocators(statep);
		    statep->progress = EMTP_PROGRESS_PARSE_BODY;
		    result = EMTP_HEADER_PARSED;
		    statep->force_return = 1;
		    goto restore_return;
		default:
		    ERROR(EMTP_UNKNOWN_TAG_ERROR);
		}

		tracep = c_p;
		trace_size = trace_endp - tracep;
	    }

	    statep->fetch_size = ERTS_MT_MAX_V1_HEADER_ENTRY_SIZE;
	    break;

	case 2: /* Version 2.X ---------------------------------------------- */

	    while (trace_size >= UI8_SZ + UI16_SZ) {
		usgnd_int_16 entry_sz;
		size_t str_len;
		usgnd_int_8 tag;

		GET_UI8(tag, c_p);
		GET_UI16(entry_sz, c_p);
		NEED(entry_sz, trace_size);

		switch (tag) {
		case ERTS_MT_ALLOCATOR_HDR_TAG: {
		    usgnd_int_8 crr_prvds;
		    usgnd_int_16 a_ix, aflgs;

		    if (entry_sz
			< UI8_SZ + 3*UI16_SZ + UI8_SZ + 0 + UI8_SZ)
			ERROR(EMTP_PARSE_ERROR);

		    GET_UI16(aflgs, c_p);
		    GET_UI16(a_ix, c_p);
		    if (a_ix > statep->max_allocator_ix)
			ERROR(EMTP_PARSE_ERROR);

		    if (aflgs & ERTS_MT_ALLCTR_USD_CRR_INFO)
			statep->allocator[a_ix]->flags
			    |= EMTP_ALLOCATOR_FLAG_HAVE_USED_CARRIERS_INFO;

		    GET_UI8(str_len, c_p);

		    if (entry_sz
			< UI8_SZ + 3*UI16_SZ + UI8_SZ + str_len + UI8_SZ)
			ERROR(EMTP_PARSE_ERROR);

		    statep->allocator[a_ix]->name
			= (char *) (*statep->alloc)(str_len + 1);
		    if (!statep->allocator[a_ix]->name)
			ERROR(EMTP_NO_MEMORY_ERROR);

		    memcpy((void *) statep->allocator[a_ix]->name,
			   (void *) c_p,
			   str_len);
		    c_p += str_len;

		    statep->allocator[a_ix]->name[str_len] = '\0';

		    GET_UI8(crr_prvds, c_p);
		    if (entry_sz < (UI8_SZ
				    + 3*UI16_SZ
				    + UI8_SZ
				    + str_len
				    + UI8_SZ
				    + crr_prvds*UI16_SZ))
			ERROR(EMTP_PARSE_ERROR);
		    statep->allocator[a_ix]->carrier.no_providers
			= (usgnd_int_16) crr_prvds;
		    statep->allocator[a_ix]->carrier.provider = (usgnd_int_16 *)
			(*statep->alloc)(crr_prvds*sizeof(usgnd_int_16));
		    if (!statep->allocator[a_ix]->carrier.provider)
			ERROR(EMTP_NO_MEMORY_ERROR);
		    for (i = 0; i < crr_prvds; i++) {
			usgnd_int_16 cp_ix;
			GET_UI16(cp_ix, c_p);
			if (cp_ix > statep->max_allocator_ix)
			    ERROR(EMTP_PARSE_ERROR);
			statep->allocator[a_ix]->carrier.provider[i] = cp_ix;
		    }

		    break;
		}

		case ERTS_MT_BLOCK_TYPE_HDR_TAG: {
		    usgnd_int_16 bt_ix, a_ix;

		    if (entry_sz
			< UI8_SZ + 3*UI16_SZ + UI8_SZ + 0 + UI16_SZ)
			ERROR(EMTP_PARSE_ERROR);

		    SKIP_UI16(c_p); /* bitflags */
		    GET_UI16(bt_ix, c_p);
		    if (bt_ix > statep->max_block_type_ix)
			ERROR(EMTP_PARSE_ERROR);

		    GET_UI8(str_len, c_p);

		    if (entry_sz
			< UI8_SZ + 3*UI16_SZ + UI8_SZ + str_len + UI16_SZ)
			ERROR(EMTP_PARSE_ERROR);

		    statep->block_type[bt_ix]->name
			= (char *) (*statep->alloc)(str_len + 1);

		    if (!statep->block_type[bt_ix]->name)
			ERROR(EMTP_NO_MEMORY_ERROR);

		    memcpy((void *) statep->block_type[bt_ix]->name,
			   (void *) c_p,
			   str_len);
		    c_p += str_len;

		    statep->block_type[bt_ix]->name[str_len] = '\0';

		    GET_UI16(a_ix, c_p);

		    if (a_ix > statep->max_allocator_ix)
			ERROR(EMTP_PARSE_ERROR);

		    statep->block_type[bt_ix]->allocator = (sgnd_int_32) a_ix;
		    statep->block_type[bt_ix]->valid = 1;
		    break;
		}

		case ERTS_MT_END_OF_HDR_TAG:
		    tracep = tracep + ((size_t) entry_sz);
		    remove_unused_allocators(statep);
		    statep->progress = EMTP_PROGRESS_PARSE_BODY;
		    result = EMTP_HEADER_PARSED;
		    statep->force_return = 1;
		    goto restore_return;

		default:
		    /* Skip tags that we do not understand. */
		    break;
		}

		tracep = tracep + ((size_t) entry_sz);
		ASSERT(c_p <= tracep);
		c_p = tracep;
		trace_size = trace_endp - tracep;
	    }

	    statep->fetch_size = UI8_SZ + UI16_SZ;
	    break;
	default:  /* Not supported version --------------------------------- */
	    ASSERT(0);
	}
	
	break;
    }
    default:
	ASSERT(0);
    }

    statep->known_need = 0;
    result = EMTP_NEED_MORE_TRACE;

 restore_return:

    *tracepp = tracep;

    return result;

}


int
emtp_parse(emtp_state *statep,
	   usgnd_int_8 **tracepp, size_t *trace_lenp,
	   emtp_operation *op_start, size_t op_size, size_t *op_lenp)
{
    int result, have_all_in_overflow;
    usgnd_int_8 *tracep, *trace_endp;
    emtp_operation *op_p, *op_endp;


    have_all_in_overflow = 0;

    op_p = op_start;

    if (!statep)
	return EMTP_NO_MEMORY_ERROR;

    if (!tracepp || !trace_lenp)
	return EMTP_NO_TRACE_ERROR;

    if (*trace_lenp <= 0) {
	if (op_lenp)
	    *op_lenp = 0;
	return EMTP_NEED_MORE_TRACE;
    }

    statep->force_return = 0;

    if (statep->overflow_size) { /* Overflow from prevoius parse */
	sgnd_int_32 tsz;
	sgnd_int_32 sz;

    fetch_for_overflow:
	sz = statep->fetch_size - statep->overflow_size;
	ASSERT(sz > 0);

	if (*trace_lenp <= sz) {
	    have_all_in_overflow = 1;
	    sz = *trace_lenp;
	}

	if (sz > statep->overflow_buf_size) {	
	    size_t buf_sz = statep->overflow_size + sz;
	    void *buf = (*statep->realloc)((void *) statep->overflow, buf_sz);
	    if (!buf)
		return EMTP_NO_MEMORY_ERROR;
	    statep->overflow_buf_size = buf_sz;
	    statep->overflow = (usgnd_int_8 *) buf;
	}

	memcpy((void *) (statep->overflow + statep->overflow_size),
	       (void *) *tracepp,
	       sz);

	tsz = statep->overflow_size + sz;

	tracep = statep->overflow;
	trace_endp = statep->overflow + tsz;

	if (tsz < statep->fetch_size && statep->known_need) {
	    ASSERT(have_all_in_overflow);
	    statep->overflow_size = tsz;
	    op_endp = NULL;
	    result = EMTP_NEED_MORE_TRACE;
	    goto restore_return;
	}
    }
    else {
	tracep = *tracepp;
	trace_endp = tracep + *trace_lenp;
    }

    if (statep->progress == EMTP_PROGRESS_PARSE_BODY) {

#if !HAVE_INT_64
	if (statep->flags & ERTS_MT_64_BIT_FLAG)
	    return EMTP_NOT_SUPPORTED_64_BITS_TRACE_ERROR;
#endif

	if (op_size < sizeof(emtp_operation))
	    return EMTP_BAD_OP_SIZE_ERROR;
	if (!op_start || !op_lenp || *op_lenp < 1)
	    return EMTP_NO_OPERATIONS_ERROR;
	op_endp = (emtp_operation *) (((char *) op_start) + (*op_lenp)*op_size);

    restart_parse_body:
#if EMTP_CAN_INLINE
	if (statep->parse_body_func)
#endif
	    result = (*statep->parse_body_func)(statep,
						&tracep, trace_endp,
						&op_p, op_endp, op_size);
#if EMTP_CAN_INLINE
	else
	    result = parse_v2_body(statep,
				   &tracep, trace_endp,
				   &op_p, op_endp, op_size);
#endif
    }
    else {
    restart_parse_header:
	op_endp = NULL;
	if (statep->progress == EMTP_PROGRESS_ENDED) {
	    result = EMTP_END_OF_TRACE;
	    goto restore_return;
	}
	result = parse_header(statep, &tracep, trace_endp);
    }

    /* Check overflow */
    if (statep->overflow_size) {
	if (tracep == statep->overflow) {
	    /* Nothing parsed, i.e. less new input than 1 entry :( */
	    if (!have_all_in_overflow)
		goto fetch_for_overflow;
	    statep->overflow_size = trace_endp - tracep;
	    trace_endp = tracep = *tracepp + *trace_lenp;
	}
	else {
	    size_t sz = tracep - (statep->overflow + statep->overflow_size);

	    ASSERT(sz > 0);

	    statep->overflow_size = 0;

	    tracep = *tracepp + sz;
	    trace_endp = *tracepp + *trace_lenp;
	    ASSERT(trace_endp >= tracep);
	    if (!statep->force_return && (trace_endp - tracep)) {
		if (statep->progress == EMTP_PROGRESS_PARSE_BODY)
		    goto restart_parse_body;
		else
		    goto restart_parse_header;
	    }
	    /* else: got it all in the overflow buffer */
	}
    }
    else {
	size_t sz = trace_endp - tracep;
	if (!statep->force_return && sz) {
	    if (sz >= statep->fetch_size) {
		ASSERT(0);
		ERROR(EMTP_PARSE_ERROR);
	    }
	    if (sz > statep->overflow_buf_size) {
		(*statep->free)((void *) statep->overflow);
		statep->overflow = (usgnd_int_8 *) (*statep->alloc)(sz);
		if (!statep->overflow) {
		    statep->overflow_buf_size = 0;
		    return EMTP_NO_MEMORY_ERROR;
		}
		statep->overflow_buf_size = sz;
	    }
	    memcpy((void *) statep->overflow, tracep, sz);
	    statep->overflow_size = sz;
	    ASSERT(tracep + sz == trace_endp);
	    tracep = trace_endp;
	}
    }

 restore_return:
    ASSERT(trace_endp >= tracep);

    *tracepp = tracep;
    *trace_lenp = trace_endp - tracep;

    if (op_lenp && op_size > 0)
	*op_lenp = (int) (((char *) op_p) - ((char *) op_start))/op_size;

    return result;
}

#ifdef DEBUG
static void
hexdump(void *start, void *end)
{
    unsigned char *p = (unsigned char *) start;

    fprintf(stderr, "hexdump: ");
    while ((void *) p < end) {
	fprintf(stderr, "%x", (unsigned) *p);
	p++;
    }
    fprintf(stderr, "\n");
}

#if PRINT_PARSED_OP
static void
print_op(emtp_operation *op_p)
{
    switch (op_p->type) {
    case EMTP_ALLOC:
	fprintf(stderr,
		"alloc: "
		"type=%" USGND_INT_16_FSTR ", "
		"ptr=%" USGND_INT_MAX_FSTR ", "
		"sz=%" USGND_INT_MAX_FSTR ", "
		"(secs=%" USGND_INT_32_FSTR ",  usecs=%" USGND_INT_32_FSTR ")"
		"\n",
		op_p->u.block.type,
		op_p->u.block.new_ptr,
		op_p->u.block.new_size,
		op_p->time.secs,
		op_p->time.usecs);
	break;
    case EMTP_REALLOC:
	fprintf(stderr,
		"realloc: "
		"type=%" USGND_INT_16_FSTR ", "
		"ptr=%" USGND_INT_MAX_FSTR ", "
		"prev_ptr=%" USGND_INT_MAX_FSTR ", "
		"sz=%" USGND_INT_MAX_FSTR ", "
		"(secs=%" USGND_INT_32_FSTR ",  usecs=%" USGND_INT_32_FSTR ")"
		"\n",
		op_p->u.block.type,
		op_p->u.block.new_ptr,
		op_p->u.block.prev_ptr,
		op_p->u.block.new_size,
		op_p->time.secs,
		op_p->time.usecs);
	break;
    case EMTP_FREE:
	fprintf(stderr,
		"free: "
		"type=%" USGND_INT_16_FSTR ", "
		"ptr=%" USGND_INT_MAX_FSTR ", "
		"(secs=%" USGND_INT_32_FSTR ",  usecs=%" USGND_INT_32_FSTR ")"
		"\n",
		op_p->u.block.type,
		op_p->u.block.prev_ptr,
		op_p->time.secs,
		op_p->time.usecs);
	break;
    case EMTP_CARRIER_ALLOC:
	fprintf(stderr,
		"carrier_alloc: "
		"type=%" USGND_INT_16_FSTR ", "
		"carrier_type=%" USGND_INT_16_FSTR ", "
		"ptr=%" USGND_INT_MAX_FSTR ", "
		"sz=%" USGND_INT_MAX_FSTR ", "
		"(secs=%" USGND_INT_32_FSTR ",  usecs=%" USGND_INT_32_FSTR ")"
		"\n",
		op_p->u.block.type,
		op_p->u.block.carrier_type,
		op_p->u.block.new_ptr,
		op_p->u.block.new_size,
		op_p->time.secs,
		op_p->time.usecs);
	break;
    case EMTP_CARRIER_REALLOC:
	fprintf(stderr,
		"carrier_realloc: "
		"type=%" USGND_INT_16_FSTR ", "
		"carrier_type=%" USGND_INT_16_FSTR ", "
		"ptr=%" USGND_INT_MAX_FSTR ", "
		"prev_ptr=%" USGND_INT_MAX_FSTR ", "
		"sz=%" USGND_INT_MAX_FSTR ", "
		"(secs=%" USGND_INT_32_FSTR ",  usecs=%" USGND_INT_32_FSTR ")"
		"\n",
		op_p->u.block.type,
		op_p->u.block.carrier_type,
		op_p->u.block.new_ptr,
		op_p->u.block.prev_ptr,
		op_p->u.block.new_size,
		op_p->time.secs,
		op_p->time.usecs);
	break;
    case EMTP_CARRIER_FREE:
	fprintf(stderr,
		"carrier_free: "
		"type=%" USGND_INT_16_FSTR ", "
		"carrier_type=%" USGND_INT_16_FSTR ", "
		"ptr=%" USGND_INT_MAX_FSTR ", "
		"(secs=%" USGND_INT_32_FSTR ",  usecs=%" USGND_INT_32_FSTR ")"
		"\n",
		op_p->u.block.type,
		op_p->u.block.carrier_type,
		op_p->u.block.prev_ptr,
		op_p->time.secs,
		op_p->time.usecs);
	break;
    case EMTP_STOP:
	fprintf(stderr,
		"stop: "
		"(secs=%" USGND_INT_32_FSTR ",  usecs=%" USGND_INT_32_FSTR ")"
		"\n",
		op_p->time.secs,
		op_p->time.usecs);
	break;
    case EMTP_EXIT:
	fprintf(stderr,
		"exit: "
		"status=%" USGND_INT_32_FSTR ", "
		"(secs=%" USGND_INT_32_FSTR ",  usecs=%" USGND_INT_32_FSTR ")"
		"\n",
		op_p->u.exit_status,
		op_p->time.secs,
		op_p->time.usecs);
	break;
    default:
	fprintf(stderr, "Unknown op type: %d\n", op_p->type);
	break;
    }
}

#endif
#endif
