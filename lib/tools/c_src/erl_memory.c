/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

/* Headers to include ... */

#ifdef __WIN32__
#	include <winsock2.h>
#	undef WIN32_LEAN_AND_MEAN
#	define WIN32_LEAN_AND_MEAN
#	include <windows.h>
typedef int socklen_t;
#else
#	if defined(__linux__) && defined(__GNUC__)
#   		define _GNU_SOURCE 1
#	endif
#	include <unistd.h>
#	include <sys/types.h>
#	include <sys/socket.h>
#	include <netinet/in.h>
#	include <fcntl.h>
#	include <netdb.h>
#	include <arpa/inet.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>

#include "erl_fixed_size_int_types.h"
#include "erl_memory_trace_parser.h"
#include "erl_memory_trace_block_table.h"
#include "ethread.h"

/* Increment when changes are made */
#define EMEM_VSN_STR "0.9"

/* Features not fully implemented yet */
#define EMEM_A_SWITCH 0
#define EMEM_C_SWITCH 0
#define EMEM_c_SWITCH 0
#define EMEM_d_SWITCH 0

/* Some system specific defines ... */
#ifdef __WIN32__
#	define ssize_t int
#	define GET_SOCK_ERRNO() (WSAGetLastError() - WSABASEERR)
#	define IS_INVALID_SOCKET(X) ((X) == INVALID_SOCKET)
#	ifdef __GNUC__
#		define INLINE __inline__
#	else
#	define INLINE __forceinline
#       endif
#	define DIR_SEP_CHAR '\\'
#else
#	define SOCKET int
#	define closesocket close
#	define GET_SOCK_ERRNO() (errno ? errno : INT_MAX)
#	define INVALID_SOCKET (-1)
#	define IS_INVALID_SOCKET(X) ((X) < 0)
#	ifdef __GNUC__
#		define INLINE __inline__
#	else
#		define INLINE
#	endif
#	define DIR_SEP_CHAR '/'
#endif

#define EM_ERL_CMD_FILE_NAME "erl_cmd.txt"
#define EM_OUTPUT_FILE_SUFFIX ".emem"

#define PRINT_OPERATIONS 0

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

#define ERR_RET(X) return (X)
#if 1
#  undef  ERR_RET
#  define ERR_RET(X) abort()
#endif

/* #define HARD_DEBUG */


#define EM_EXIT_RESULT			(EMTBT_MIN_ERROR - 1)
#define EM_TRUNCATED_TRACE_ERROR	(EMTBT_MIN_ERROR - 2)
#define EM_INTERNAL_ERROR		(EMTBT_MIN_ERROR - 3)

#define EM_DEFAULT_BUF_SZ 8192

#define EM_LINES_UNTIL_HEADER 20
#define EM_NO_OF_OPS 400
#define EM_MAX_CONSECUTIVE_TRACE_READS 10
#define EM_MAX_NO_OF_TRACE_BUFS 1280
#define EM_MIN_TRACE_READ_SIZE (EM_DEFAULT_BUF_SZ/20)
#define EM_TIME_FIELD_WIDTH 11

static void error(int res);
static void error_msg(int res, char *msg);

typedef struct {
    usgnd_int_max size;
    usgnd_int_max min_size;
    usgnd_int_max max_size;
    usgnd_int_max max_ever_size;
    usgnd_int_max no;
    usgnd_int_max min_no;
    usgnd_int_max max_no;
    usgnd_int_max max_ever_no;
    usgnd_int_max allocs;
    usgnd_int_max reallocs;
    usgnd_int_max frees;
} em_mem_info;

typedef struct em_buffer_ {
    struct em_buffer_ *next;
    int write;
    char *data;
    char *data_end;
    char *end;
    size_t size;
    char start[EM_DEFAULT_BUF_SZ];
} em_buffer;

typedef struct {
    int no_writer;
    int no_reader;
    size_t tot_buf_size;
    size_t max_buf_size;
    char *name;
    em_buffer *first;
    em_buffer *last;
    ethr_mutex mutex;
    ethr_cond cond;
    int used_def_buf_a;
    em_buffer def_buf_a;
    int used_def_buf_b;
    em_buffer def_buf_b;
} em_buf_queue;

typedef struct {
    char *ptr;
    size_t size;
} em_area;

typedef struct {
    char *name;
    int ix;
} em_output_types;

typedef struct {

    /* Memory allocation functions */
    void * (*alloc)(size_t);
    void * (*realloc)(void *, size_t);
    void   (*free)(void *);

    emtbt_table *block_table;
    emtbt_table **carrier_table;

    struct {
	em_mem_info total;
	em_mem_info *btype;
	em_mem_info *allctr;
	em_mem_info **allctr_prv_crr;
	em_mem_info **allctr_usd_crr;

	struct {
	    usgnd_int_32 secs;
	    usgnd_int_32 usecs;
	} stop_time;
	emtp_op_type stop_reason;
	usgnd_int_32 exit_status;
    } info;

    /* Input ... */
    struct {
	usgnd_int_16 listen_port;
	SOCKET socket;
	usgnd_int_max total_trace_size;
	int error;
	char *error_descr;
	em_buf_queue queue;
    } input;

    /* Output ... */
    struct {
	usgnd_int_32 next_print;
	usgnd_int_32 next_print_inc;
	char *header;
	size_t header_size;
	size_t values_per_object;
	size_t values_per_line;
	size_t field_width;
	int verbose;
	int total;
	int all_allctrs;
	int no_allctrs;
	em_output_types *allctrs;
	int all_btypes;
	int no_btypes;
	em_output_types *btypes;
	int max_min_values;
	int block_counts;
	int op_counts;
	int lines_until_header;
	FILE *stream;
	char *file_name;
#if EMEM_d_SWITCH
	char *dir_name;
	FILE *erl_cmd_file;
	struct {
	    ethr_mutex *mutex;
	    ethr_cond *cond;
	} go;
#endif
	em_buf_queue queue;
    } output;

    /* Trace info */
    emtp_state *trace_state;
    emtp_info  trace_info;

} em_state;

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Threads...                                                              *
 *                                                                         *
\*                                                                         */

static INLINE void
mutex_init(ethr_mutex *mtx)
{
    int res = ethr_mutex_init(mtx);
    if (res)
	error_msg(res, "Mutex init");
}

static INLINE void
mutex_destroy(ethr_mutex *mtx)
{
    int res = ethr_mutex_destroy(mtx);
    if (res)
	error_msg(res, "Mutex destroy");
}

static INLINE void
mutex_lock(ethr_mutex *mtx)
{
    ethr_mutex_lock(mtx);
}

static INLINE void
mutex_unlock(ethr_mutex *mtx)
{
    ethr_mutex_unlock(mtx);
}

static INLINE void
cond_init(ethr_cond *cnd)
{
    int res = ethr_cond_init(cnd);
    if (res)
	error_msg(res, "Cond init");
}

static INLINE void
cond_destroy(ethr_cond *cnd)
{
    int res = ethr_cond_destroy(cnd);
    if (res)
	error_msg(res, "Cond destroy");
}

static INLINE void
cond_wait(ethr_cond *cnd, ethr_mutex *mtx)
{
    int res = ethr_cond_wait(cnd, mtx);
    if (res != 0 && res != EINTR)
	error_msg(res, "Cond wait");
}

static INLINE void
cond_signal(ethr_cond *cnd)
{
    ethr_cond_signal(cnd);
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Buffer queues                                                           *
 *                                                                         *
\*                                                                         */

static INLINE void
reset_buffer(em_buffer *b, size_t size)
{
    b->write = 1;
    b->next = NULL;
    if (size) {
	b->size = size;
	b->end = b->start + size;
    }
    b->data_end = b->data = b->start;
}

static void
init_queue(em_state *state, em_buf_queue *queue)
{
    reset_buffer(&queue->def_buf_a, EM_DEFAULT_BUF_SZ);
    reset_buffer(&queue->def_buf_b, EM_DEFAULT_BUF_SZ);
    queue->first = NULL;
    queue->last = NULL;
    queue->no_writer = 0;
    queue->no_reader = 0;
    queue->tot_buf_size = 0;
    queue->max_buf_size = ~0;
    queue->name = "";
    queue->used_def_buf_a = 0;
    queue->used_def_buf_b = 0;
    mutex_init(&queue->mutex);
    cond_init(&queue->cond);
}

static void
destroy_queue(em_state *state, em_buf_queue *queue)
{
    while (queue->first) {
	em_buffer *buf = queue->first;
	queue->first = queue->first->next;
	if (buf != &queue->def_buf_a && buf != &queue->def_buf_b)
	    (*state->free)((void *) buf);
    }
    mutex_destroy(&queue->mutex);
    cond_destroy(&queue->cond);
}

static void
disconnect_queue_writer(em_buf_queue *queue)
{
    mutex_lock(&queue->mutex);
    queue->no_writer = 1;
    cond_signal(&queue->cond);
    mutex_unlock(&queue->mutex);
}

static void
disconnect_queue_reader(em_buf_queue *queue)
{
    mutex_lock(&queue->mutex);
    queue->no_reader = 1;
    cond_signal(&queue->cond);
    mutex_unlock(&queue->mutex);
}

static int
is_queue_writer_disconnected(em_buf_queue *queue)
{
    int res;
    mutex_lock(&queue->mutex);
    res = queue->no_writer;
    mutex_unlock(&queue->mutex);
    return res;
}

static int
is_queue_reader_disconnected(em_buf_queue *queue)
{
    int res;
    mutex_lock(&queue->mutex);
    res = queue->no_reader;
    mutex_unlock(&queue->mutex);
    return res;
}

static INLINE void
dequeue(em_state *state, em_buf_queue *queue)
{
    em_buffer *buf;

    ASSERT(queue->first);
    ASSERT(queue->tot_buf_size > 0);

    buf = queue->first;
    queue->first = buf->next;
    if (!queue->first)
	queue->last = NULL;

    ASSERT(queue->tot_buf_size >= buf->size);
    queue->tot_buf_size -= buf->size;

    if (buf == &queue->def_buf_a)
	queue->used_def_buf_a = 0;
    else if (buf == &queue->def_buf_b)
	queue->used_def_buf_b = 0;
    else
	(*state->free)((void *) buf);

}


static INLINE em_buffer *
enqueue(em_state *state, em_buf_queue *queue, size_t min_size)
{
    em_buffer *buf;

    if (min_size > EM_DEFAULT_BUF_SZ)
	goto alloc_buf;

    if (!queue->used_def_buf_a) {
	buf = &queue->def_buf_a;
	queue->used_def_buf_a = 1;
	reset_buffer(buf, 0);
    }
    else if (!queue->used_def_buf_b) {
	buf = &queue->def_buf_b;
	queue->used_def_buf_b = 1;
	reset_buffer(buf, 0);
    }
    else {
	size_t bsize;
    alloc_buf:

	bsize = EM_DEFAULT_BUF_SZ;
	if (bsize < min_size)
	    bsize = min_size;
	
	buf = (em_buffer *) (*state->alloc)(sizeof(em_buffer)
					    + (sizeof(char)
					       * (bsize-EM_DEFAULT_BUF_SZ)));
	if (buf) {
	    buf->size = bsize;
	    reset_buffer(buf, bsize);
	}
    }

    if (queue->last) {
	ASSERT(queue->first);
	queue->last->write = 0;
	queue->last->next = buf;
    }
    else {
	ASSERT(!queue->first);
	queue->first = buf;
    }

    queue->tot_buf_size += buf->size;
    queue->last = buf;

    return buf;
}

static void
get_next_read_area(em_area *area, em_state *state, em_buf_queue *queue)
{
    mutex_lock(&queue->mutex);

    while (!queue->first || queue->first->data == queue->first->data_end) {
	if (queue->first && (!queue->first->write
			     || queue->first->data == queue->first->end)) {
	    dequeue(state, queue);
	    continue;
	}

	if (queue->no_writer) {
	    area->ptr = NULL;
	    area->size = 0;
	    mutex_unlock(&queue->mutex);
	    return;
	}
	cond_wait(&queue->cond, &queue->mutex);
    }

    ASSERT(queue->first->data < queue->first->data_end);

    area->ptr = queue->first->data;
    area->size =  queue->first->data_end - queue->first->data;

    queue->first->data = queue->first->data_end;

    mutex_unlock(&queue->mutex);
}

static INLINE void
wrote_area_aux(em_area *area, em_state *state, em_buf_queue *queue, int do_lock)
{
    em_buffer *buf;

    if (do_lock)
	mutex_lock(&queue->mutex);

    buf = queue->last;

    ASSERT(area->ptr);
    ASSERT(area->size);

    ASSERT(buf);
    ASSERT(buf->data_end == area->ptr);
    ASSERT(buf->end >= area->ptr + area->size);

    buf->data_end = area->ptr + area->size;

    area->ptr = NULL;
    area->size = 0;

    cond_signal(&queue->cond);

    if (do_lock)
	mutex_unlock(&queue->mutex);
}

static INLINE void
wrote_area(em_area *area, em_state *state, em_buf_queue *queue)
{
    wrote_area_aux(area, state, queue, 1);
}

static void
get_next_write_area(em_area *area, em_state *state, em_buf_queue *queue,
		    size_t size)
{
    em_buffer *buf;

    mutex_lock(&queue->mutex);

    ASSERT(!area->size || area->ptr); 

    if (area->size)
	wrote_area_aux(area, state, queue, 0);

    buf = ((queue->last && queue->last->end - queue->last->data_end >= size)
	   ? queue->last
	   : enqueue(state, queue, size));

    if (buf) {
	ASSERT(buf->end - buf->data_end >= size);
	area->ptr = buf->data_end;
	area->size = buf->end - buf->data_end;
    }
    else {
	area->ptr = NULL;
	area->size = 0;
    }

    if (queue->tot_buf_size > queue->max_buf_size) {
	fprintf(stderr,
		"emem: Maximum %s buffer size (%lu) exceeded. "
		"Terminating...\n",
		queue->name,
		(unsigned long) queue->max_buf_size);
	exit(1);
    }

    mutex_unlock(&queue->mutex);

}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Output                                                                  *
 *                                                                         *
\*                                                                         */

static INLINE size_t
write_str(char **dstpp, char *srcp)
{
    size_t i = 0;
    if (dstpp)
	while (srcp[i])
	    *((*dstpp)++) = srcp[i++];
    else
	while (srcp[i]) i++;
    return i;
}


static size_t
write_strings(char **ptr,
	      char **strings,
	      char *first_line_prefix,
	      char *line_prefix,
	      size_t max_line_size)
{
    size_t size;
    size_t tot_size = 0;
    size_t line_size = 0;
    size_t line_prefix_size;
    sgnd_int_32 ix;

    tot_size = line_size = line_prefix_size = write_str(ptr, first_line_prefix);

    for (ix = 0; strings[ix]; ix++) {
	size = write_str(NULL, strings[ix]);
	if (line_size + 1 + size > max_line_size) {
	    tot_size += write_str(ptr, "\n");
	    tot_size += write_str(ptr, line_prefix);
	    line_size = line_prefix_size;
	}
	tot_size += write_str(ptr, " ");
	tot_size += ptr ? write_str(ptr, strings[ix]) : size;
	line_size += 1 + size;
    }

    tot_size += write_str(ptr, "\n");

    return tot_size;
}

static size_t
write_title(char **bufp, size_t *overflow, size_t width, char *str)
{
    size_t i, sz, ws;
    char *p, *endp;

    /*
     * Writes at least one '|' character at the beginning.
     * Right aligns "str".
     * If "str" is larger than "width - 1" and overflow is NULL,
     * then "str" is trucated; otherwise, string is not truncated.
     */

    if (width <= 0)
	return 0;

    if (!bufp && !overflow)
	return width;

    sz = strlen(str) + 1;
    if (sz > width) {
	ws = 0;
	if (overflow)
	    *overflow += sz - width;
	else
	    sz = width;
    }
    else {
	ws = width - sz;
	if (overflow) {
	    if (ws >= *overflow) {
		ws -= *overflow;
		*overflow = 0;
	    }
	    else {
		*overflow -= ws;
		ws = 0;
	    }
	}
	sz += ws;
    }
    if (!bufp)
	return sz;

    p = *bufp;
    endp = p + width;

    *(p++) = '|';
    while (ws > 1) {
	ws--;
	*(p++) = ' ';
    }

    i = 0;
    while (str[i] && (overflow || p < endp))
	*(p++) = str[i++];

    while (ws) {
	ws--;
	*(p++) = ' ';
    }

    ASSERT(overflow || p == endp);
    ASSERT(sz == (size_t) (p - *bufp));
    *bufp = p;
    return sz;
}

static size_t
write_obj_sub_titles(em_state *state, char **bufp, size_t *overflow)
{
    size_t field_width = state->output.field_width;
    size_t size = write_title(bufp, overflow, field_width, "size");
    if (state->output.max_min_values) {
	size += write_title(bufp, overflow, field_width, "min size");
	size += write_title(bufp, overflow, field_width, "max size");
    }
    if (state->output.block_counts) {
	size += write_title(bufp, overflow, field_width, "no");
	if (state->output.max_min_values) {
	    size += write_title(bufp, overflow, field_width,  "min no");
	    size += write_title(bufp, overflow, field_width,  "max no");
	}
    }
    if (state->output.op_counts) {
	size += write_title(bufp, overflow, field_width, "alloc()");
	size += write_title(bufp, overflow, field_width, "realloc()");
	size += write_title(bufp, overflow, field_width, "free()");
    }

    return size;
}

static size_t
write_header(em_state *state, char *ptr, int trunc)
{
#define MIN_LTEXT_SZ 18
#define HEADER_EOL_STR "|\n"
    char *p;
    char **pp;
    int i;
    size_t overflow;
    size_t *ofp;
    size_t obj_size = state->output.values_per_object*state->output.field_width;
    size_t size = 0;
    int have_seg_crr = state->trace_info.have_segment_carrier_info;

    if (ptr) {
	p = ptr;
	pp = &p;
    }
    else {
	p = NULL;
	pp = NULL;
    }

    overflow = 0;
    ofp = trunc ? NULL : &overflow;

    size += write_title(pp, ofp, EM_TIME_FIELD_WIDTH, "time");

    if (state->output.total) {
	int no = 1;
	if (have_seg_crr) {
	    if (state->info.allctr_prv_crr[state->trace_info.segment_ix])
		no++;
	    if (state->info.allctr_usd_crr[state->trace_info.segment_ix])
		no++;
	}
	size += write_title(pp, ofp, (have_seg_crr ? 3 : 1)*obj_size, "total");
    }

    for (i = 0; i < state->output.no_allctrs; i++) {
	int no = 1;
	if (state->info.allctr_prv_crr[state->output.allctrs[i].ix])
	    no++;
	if (state->info.allctr_usd_crr[state->output.allctrs[i].ix])
	    no++;
	size += write_title(pp, ofp, no*obj_size, state->output.allctrs[i].name);
    }

    for (i = 0; i < state->output.no_btypes; i++)
	size += write_title(pp, ofp, obj_size, state->output.btypes[i].name);

    size += write_str(pp, HEADER_EOL_STR);

    overflow = 0;
    size += write_title(pp, ofp, EM_TIME_FIELD_WIDTH, "");

    if (state->output.total) {
	size += write_title(pp, ofp, obj_size, (obj_size <= MIN_LTEXT_SZ
						? "alcd blks"
						: "allocated blocks"));
	if (have_seg_crr) {
	    if (state->info.allctr_prv_crr[state->trace_info.segment_ix])
		size += write_title(pp, ofp, obj_size, (obj_size <= MIN_LTEXT_SZ
							? "mpd segs"
							: "mapped segments"));
	    if (state->info.allctr_usd_crr[state->trace_info.segment_ix])
		size += write_title(pp, ofp, obj_size, (obj_size <= MIN_LTEXT_SZ
							? "chd segs"
							: "cached segments"));
	}
    }

    for (i = 0; i < state->output.no_allctrs; i++) {
	size += write_title(pp, ofp, obj_size, (obj_size <= MIN_LTEXT_SZ
						? "alcd blks"
						: "allocated blocks"));
	if (state->info.allctr_prv_crr[state->output.allctrs[i].ix])
	    size += write_title(pp, ofp, obj_size, (obj_size <= MIN_LTEXT_SZ
						    ? "prvd crrs" 
						    : "provided carriers"));
	if (state->info.allctr_usd_crr[state->output.allctrs[i].ix])
	    size += write_title(pp, ofp, obj_size, (obj_size <= MIN_LTEXT_SZ
						    ? "usd crrs"
						    : "used carriers"));
    }
    for (i = 0; i < state->output.no_btypes; i++)
	size += write_title(pp, ofp, obj_size, (obj_size <= MIN_LTEXT_SZ
						? "alcd blks"
						: "allocated blocks"));


    size += write_str(pp, HEADER_EOL_STR);
    overflow = 0;
    size += write_title(pp, ofp, EM_TIME_FIELD_WIDTH, ""); 

    if (state->output.total) {
	size += write_obj_sub_titles(state, pp, ofp);
	if (have_seg_crr) {
	    if (state->info.allctr_prv_crr[state->trace_info.segment_ix])
		size += write_obj_sub_titles(state, pp, ofp);
	    if (state->info.allctr_usd_crr[state->trace_info.segment_ix])
		size += write_obj_sub_titles(state, pp, ofp);
	}
    }

    for (i = 0; i < state->output.no_allctrs; i++) {
	size += write_obj_sub_titles(state, pp, ofp);
	if (state->info.allctr_prv_crr[state->output.allctrs[i].ix])
	    size += write_obj_sub_titles(state, pp, ofp);
	if (state->info.allctr_usd_crr[state->output.allctrs[i].ix])
	    size += write_obj_sub_titles(state, pp, ofp);
    }

    for (i = 0; i < state->output.no_btypes; i++)
	size += write_obj_sub_titles(state, pp, ofp);

    size += write_str(pp, HEADER_EOL_STR);
#undef MIN_LTEXT_SZ
#undef HEADER_EOL_STR
    return size;
}

static INLINE void
write_mem_info(em_state *state, char **p, em_mem_info *mi)
{
    int fw = state->output.field_width - 1;
    *p += sprintf(*p, "%*" USGND_INT_MAX_FSTR " ", fw, mi->size);
    if (state->output.max_min_values)
	*p += sprintf(*p,
		      "%*" USGND_INT_MAX_FSTR
		      " %*" USGND_INT_MAX_FSTR " ",
		      fw, mi->min_size,
		      fw, mi->max_size);
    if (state->output.block_counts) {
	*p += sprintf(*p, "%*" USGND_INT_MAX_FSTR " ", fw, mi->no);
	if (state->output.max_min_values)
	    *p += sprintf(*p,
			  "%*" USGND_INT_MAX_FSTR
			  " %*" USGND_INT_MAX_FSTR " ",
			  fw, mi->min_no,
			  fw, mi->max_no);
    }
    if (state->output.op_counts)
	*p += sprintf(*p,
		      "%*" USGND_INT_MAX_FSTR
		      " %*" USGND_INT_MAX_FSTR
		      " %*" USGND_INT_MAX_FSTR " ",
		      fw, mi->allocs,
		      fw, mi->reallocs,
		      fw, mi->frees);

    /* Update max ever values */
    if (mi->max_ever_size < mi->max_size)
	mi->max_ever_size = mi->max_size;
    if (mi->max_ever_no < mi->max_no)
	mi->max_ever_no = mi->max_no;
    /* Reset max/min values */
    mi->max_size = mi->min_size = mi->size;
    mi->max_no = mi->min_no = mi->no;
}

static INLINE void
write_max_ever_mem_info(em_state *state, char **p, em_mem_info *mi)
{
    int fw = state->output.field_width - 1;
    *p += sprintf(*p, "%*" USGND_INT_MAX_FSTR " ", fw, mi->max_ever_size);
    if (state->output.max_min_values)
	*p += sprintf(*p, "%*s %*s ", fw, "", fw, "");
    if (state->output.block_counts) {
	*p += sprintf(*p, "%*" USGND_INT_MAX_FSTR " ", fw, mi->max_ever_no);
	if (state->output.max_min_values)
	    *p += sprintf(*p, "%*s %*s ", fw, "", fw, "");
    }
    if (state->output.op_counts)
	*p += sprintf(*p, "%*s %*s %*s ", fw, "", fw, "", fw, "");
}

static void
print_string(em_state *state, char *str)
{
    em_area area = {NULL, 0};
    char *p;

    /* Get area */

    get_next_write_area(&area,state,&state->output.queue,write_str(NULL,str));

    p = area.ptr;
    area.size = write_str(&p, str);

    /* Leave area */

    wrote_area(&area, state, &state->output.queue);

}

static int
print_emu_arg(em_state *state)
{
    em_area area = {NULL, 0};
    char hostname[100];
    char carg[22];
    struct sockaddr_in saddr;
    struct hostent *hp;
    struct in_addr iaddr;
    usgnd_int_16 port;
    socklen_t saddr_size = sizeof(saddr);
    size_t size;
    char *format = "> Emulator command line argument: +Mit %s\n";

    if (getsockname(state->input.socket,
		    (struct sockaddr *) &saddr,
		    &saddr_size) != 0)
	goto error;
    
    port = ntohs(saddr.sin_port);

    ASSERT(state->input.listen_port == 0 || state->input.listen_port == port);

    state->input.listen_port = port;

    if (gethostname(hostname, sizeof(hostname)) != 0)
	goto error;

    hp = gethostbyname(hostname);
    if (!hp)
	goto error;

    if (hp->h_addr_list) {
	(void) memcpy(&iaddr.s_addr, *hp->h_addr_list, sizeof(iaddr.s_addr));
	(void) sprintf(carg, "%s:%d", inet_ntoa(iaddr), (int) port);
    }
    else
	(void) sprintf(carg, "127.0.0.1:%d", (int) port);

#if EMEM_d_SWITCH

    if (state->output.erl_cmd_file) {
	fprintf(state->output.erl_cmd_file, "+Mit %s\n", carg);
	fclose(state->output.erl_cmd_file);
	state->output.erl_cmd_file = NULL;
    }

#endif

    size = strlen(format) + strlen(carg);

    /* Get area */

    get_next_write_area(&area, state, &state->output.queue, size);

    area.size = sprintf(area.ptr, format, carg);

    /* Leave area */

    wrote_area(&area, state, &state->output.queue);

    return 0;

 error:
    return GET_SOCK_ERRNO();
}

static size_t
write_allocator_info(em_state *state, char *ptr)
{
    usgnd_int_32 aix, i, j;
    char *header =	"> Allocator information:\n";
    char *allctr_str =	">    * Allocator:";
    char *crr_prv_str =	">       * Carrier providers:";
    char *blk_tp_str =	">       * Block types:";
    char *line_prefix =	">         ";
    size_t size = 0;
    char **strings;
    size_t strings_size;
    size_t max_line_size = 80;
    char *p = ptr;
    char **pp = ptr ? &p : NULL;

    strings_size = state->trace_info.max_block_type_ix + 1;
    if (strings_size < state->trace_info.max_allocator_ix + 1)
	strings_size = state->trace_info.max_allocator_ix + 1;

    strings = (char **) (*state->alloc)((strings_size + 1)*sizeof(char *));
    if (!strings)
	error(ENOMEM);

    size += write_str(pp, header);

    for (aix = 0; aix <= state->trace_info.max_allocator_ix; aix++) {
	emtp_allocator *allctr = state->trace_info.allocator[aix];
	if (!allctr->valid)
	    continue;

	strings[0] = allctr->name;
	strings[1] = NULL;
	size += write_strings(pp,strings,allctr_str,line_prefix,max_line_size);

	i = 0;
	if (allctr->carrier.provider)
	    for (j = 0; j < allctr->carrier.no_providers; j++) {
		usgnd_int_32 cpix = allctr->carrier.provider[j];
		if (cpix == state->trace_info.segment_ix)
		    strings[i++] = "segment";
		else
		    strings[i++] = state->trace_info.allocator[cpix]->name;
	    }
	strings[i] = NULL;
	size += write_strings(pp,strings,crr_prv_str,line_prefix,max_line_size);
	
	i = 0;
	for (j = 0; j <= state->trace_info.max_block_type_ix; j++)
	    if (state->trace_info.block_type[j]->allocator == aix)
		strings[i++] = state->trace_info.block_type[j]->name;
	strings[i] = NULL;
	size += write_strings(pp,strings,blk_tp_str,line_prefix,max_line_size);
    }

    (*state->free)((void *) strings);

    return size;
}

static void
print_main_header(em_state *state)
{
#if HAVE_INT_64
#define MAX_WORD_SZ_STR "64"
#else
#define MAX_WORD_SZ_STR "32"
#endif
    em_area area = {NULL, 0};
    char *format1 =
	"> emem version:            " EMEM_VSN_STR "\n"
	"> Nodename:                %s\n"
	"> Hostname:                %s\n"
	"> Pid:                     %s\n"
	"> Start time (UTC):        ";
    char *format2 = "%4.4" USGND_INT_32_FSTR
	"-%2.2" USGND_INT_32_FSTR "-%2.2" USGND_INT_32_FSTR
	" %2.2" USGND_INT_32_FSTR ":%2.2" USGND_INT_32_FSTR
	":%2.2" USGND_INT_32_FSTR ".%6.6" USGND_INT_32_FSTR "\n";
    char *format3 =
	"> Trace parser version:    %" USGND_INT_32_FSTR ".%" USGND_INT_32_FSTR
	    "\n"
	"> Actual trace version:    %" USGND_INT_32_FSTR ".%" USGND_INT_32_FSTR
	    "\n"
	"> Maximum trace word size: " MAX_WORD_SZ_STR " bits\n"
	"> Actual trace word size:  %d bits\n";
    size_t size = (strlen(format1) +
		   (state->trace_info.start_time.month
		    ? (strlen(format2) + 7*10)
		    : 1)
		   + strlen(format3)
		   + strlen(state->trace_info.nodename)
		   + strlen(state->trace_info.hostname)
		   + strlen(state->trace_info.pid)
		   + 5*10 + 1);

    if (state->output.verbose) {
	size += write_allocator_info(state, NULL);
    }

    size += write_header(state, NULL, 0);

    /* Get area */
    get_next_write_area(&area, state, &state->output.queue, size);

    area.size = sprintf(area.ptr,
			format1,
			state->trace_info.nodename,
			state->trace_info.hostname,
			state->trace_info.pid);
    if (state->trace_info.start_time.month)
	area.size += sprintf(area.ptr + area.size,
			     format2,
			     state->trace_info.start_time.year,
			     state->trace_info.start_time.month,
			     state->trace_info.start_time.day,
			     state->trace_info.start_time.hour,
			     state->trace_info.start_time.minute,
			     state->trace_info.start_time.second,
			     state->trace_info.start_time.micro_second);
    else
	*(area.ptr + area.size++) = '\n';
    area.size += sprintf(area.ptr + area.size,
			 format3,
			 state->trace_info.version.parser.major,
			 state->trace_info.version.parser.minor,
			 state->trace_info.version.trace.major,
			 state->trace_info.version.trace.minor,
			 state->trace_info.bits);

    if (state->output.verbose) {
	area.size += write_allocator_info(state, area.ptr + area.size);
    }

    area.size += write_header(state, area.ptr + area.size, 0);

    /* Leave area */
    wrote_area(&area, state, &state->output.queue);
#undef MAX_WORD_SZ_STR
}

static void
print_main_footer(em_state *state)
{
    em_area area = {NULL, 0};
    char *p;
    int i;
    char *stop_str =
	"> Trace stopped\n";
    char *exit_str =
	"> Emulator exited with code: %" USGND_INT_32_FSTR "\n";
    char *format =
	"> Total trace size:          %" USGND_INT_MAX_FSTR " bytes\n"
	"> Average band width used:   %" USGND_INT_MAX_FSTR " Kbit/s\n";
    size_t size;
    usgnd_int_max tsz = state->input.total_trace_size;
    usgnd_int_32 secs = state->info.stop_time.secs;
    usgnd_int_32 usecs = state->info.stop_time.usecs;
    usgnd_int_max bw;

    /* Max size of the max value line. Each value can at most use 21
       characters: largest possible usgnd_int_64 (20 digits) and one
       white space. */
    size = state->output.values_per_line*21 + 1;

    switch (state->info.stop_reason) {
    case EMTP_STOP:
	size += strlen(stop_str) + 1;
	break;
    case EMTP_EXIT:
	size += strlen(exit_str);
	size += 10; /* Enough for one unsgn_int_32 */
	size++;
	break;
    default:
	break;
    }
    size += strlen(format);
    size += 2*20; /* Enough for two unsgn_int_64 */
    size += 2;

    bw = (tsz + 1023)/1024;
    bw *= 1000;
    bw /= secs*1000 + usecs/1000;
    bw *= 8;

    /* Get area */

    get_next_write_area(&area, state, &state->output.queue, size);

    p = area.ptr;

    p += sprintf(p, "> %-*s", EM_TIME_FIELD_WIDTH - 2, "Maximum:");
    
    if (state->output.total) {
	int six = state->trace_info.segment_ix;
	write_max_ever_mem_info(state, &p, &state->info.total);
	if (state->trace_info.have_segment_carrier_info) {
	    if (state->info.allctr_prv_crr[six])
		write_max_ever_mem_info(state,
					&p,
					state->info.allctr_prv_crr[six]);
	    if (state->info.allctr_usd_crr[six])
		write_max_ever_mem_info(state,
					&p,
					state->info.allctr_usd_crr[six]);
	}
    }
    for (i = 0; i < state->output.no_allctrs; i++) {
	int ix = state->output.allctrs[i].ix;
	write_max_ever_mem_info(state, &p, &state->info.allctr[ix]);
	if (state->info.allctr_prv_crr[ix])
	    write_max_ever_mem_info(state,
				    &p,
				    state->info.allctr_prv_crr[ix]);
	if (state->info.allctr_usd_crr[ix])
	    write_max_ever_mem_info(state,
				    &p,
				    state->info.allctr_usd_crr[ix]);
    }
    for (i = 0; i < state->output.no_btypes; i++)
	write_max_ever_mem_info(state,
				&p,
				&state->info.btype[state->output.btypes[i].ix]);

    p += sprintf(p, "\n");

    switch (state->info.stop_reason) {
    case EMTP_STOP:
	p += sprintf(p, "%s", stop_str);
	break;
    case EMTP_EXIT:
	p += sprintf(p, exit_str, state->info.exit_status);
	break;
    default:
	break;
    }

    p += sprintf(p, format, tsz, bw);

    area.size = p - area.ptr;

    ASSERT(area.size <= size);

    /* Leave area */

    wrote_area(&area, state, &state->output.queue);

}

static void
print_info(em_state *state, usgnd_int_32 secs, char *extra)
{
    char *p;
    int i;
    size_t size;
    em_area area = {NULL, 0};

    /* Get area */

    size = 0;
    if (!state->output.lines_until_header)
	size += state->output.header_size;

    /* Max size of one line of values. Each value can at most use 21
       characters: largest possible usgnd_int_64 (20 digits) and one white
       space. */
    size += state->output.values_per_line*21 + 1;

    if (extra)
	size += write_str(NULL, extra);

    get_next_write_area(&area, state, &state->output.queue, size);

    /* Write to area */

    p = area.ptr;

    if (!state->output.lines_until_header) {
	memcpy((void *) area.ptr,
	       (void *) state->output.header,
	       state->output.header_size);
	p += state->output.header_size;
	state->output.lines_until_header = EM_LINES_UNTIL_HEADER;
    }
    else
	state->output.lines_until_header--;
    
    p += sprintf(p, "%*" USGND_INT_32_FSTR " ", EM_TIME_FIELD_WIDTH - 1, secs);
    
    if (state->output.total) {
	int six = state->trace_info.segment_ix;
	write_mem_info(state, &p, &state->info.total);
	if (state->trace_info.have_segment_carrier_info) {
	    if (state->info.allctr_prv_crr[six])
		write_mem_info(state, &p, state->info.allctr_prv_crr[six]);
	    if (state->info.allctr_usd_crr[six])
		write_mem_info(state, &p, state->info.allctr_usd_crr[six]);
	}
    }
    for (i = 0; i < state->output.no_allctrs; i++) {
	int ix = state->output.allctrs[i].ix;
	write_mem_info(state, &p, &state->info.allctr[ix]);
	if (state->info.allctr_prv_crr[ix])
	    write_mem_info(state, &p, state->info.allctr_prv_crr[ix]);
	if (state->info.allctr_usd_crr[ix]) 
	    write_mem_info(state, &p, state->info.allctr_usd_crr[ix]);
    }
    for (i = 0; i < state->output.no_btypes; i++)
	write_mem_info(state,
		       &p,
		       &state->info.btype[state->output.btypes[i].ix]);

    p += sprintf(p, "\n");

    if (extra)
	p += write_str(&p, extra);

    ASSERT(area.size >= p - area.ptr);
    area.size = p - area.ptr;

    /* Leave area */

    wrote_area(&area, state, &state->output.queue);
}

static void
reset_mem_info(em_mem_info *mi)
{
    mi->size = 0;
    mi->min_size = 0;
    mi->max_size = 0;
    mi->max_ever_size = 0;
    mi->no = 0;
    mi->min_no = 0;
    mi->max_no = 0;
    mi->max_ever_no = 0;
    mi->allocs = 0;
    mi->reallocs = 0;
    mi->frees = 0;
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * State creation and destruction                                          *
 *                                                                         *
\*                                                                         */

static void
destroy_state(em_state *state)
{
    int i;
    void (*freep)(void *);

    freep = state->free;

    if (state->block_table)
	emtbt_destroy_table(state->block_table);

    if (state->carrier_table) {
	for (i = -1; i <= state->trace_info.max_allocator_ix; i++)
	    if (state->carrier_table[i])
		emtbt_destroy_table(state->carrier_table[i]);
	state->carrier_table--;
	(*freep)((void *) state->carrier_table);
    }

    if (state->info.btype) {
	state->info.btype--;
	(*freep)((void *) state->info.btype);
    }

    if (state->info.allctr) {
	state->info.allctr--;
	(*freep)((void *) state->info.allctr);
    }

    if (state->info.allctr_prv_crr) {
	for (i = -1; i <= state->trace_info.max_allocator_ix; i++)
	    if (state->info.allctr_prv_crr[i])
		(*freep)((void *) state->info.allctr_prv_crr[i]);
	state->info.allctr_prv_crr--;
	(*freep)((void *) state->info.allctr_prv_crr);
    }


    if (state->info.allctr_usd_crr) {
	for (i = -1; i <= state->trace_info.max_allocator_ix; i++)
	    if (state->info.allctr_usd_crr[i])
		(*freep)((void *) state->info.allctr_usd_crr[i]);
	state->info.allctr_usd_crr--;
	(*freep)((void *) state->info.allctr_usd_crr);
    }

    emtp_state_destroy(state->trace_state);
    destroy_queue(state, &state->input.queue);

    if (state->output.btypes)
	(*freep)((void *) state->output.btypes);
    if (state->output.allctrs)
	(*freep)((void *) state->output.allctrs);
    destroy_queue(state, &state->output.queue);

#if EMEM_d_SWITCH

    if (state->output.go.mutex) {
	mutex_destroy(state->output.go.mutex);
	(*state->free)((void *) state->output.go.mutex);
	state->output.go.mutex = NULL;
    }
    if (state->output.go.cond) {
	cond_destroy(state->output.go.cond);
	(*state->free)((void *) state->output.go.cond);
	state->output.go.cond = NULL;
    }

#endif

    if (!IS_INVALID_SOCKET(state->input.socket)) {
	closesocket(state->input.socket);
        state->input.socket = INVALID_SOCKET;
    }

    (*freep)((void *) state);
}

static em_state *
new_state(void * (*alloc)(size_t),
	  void * (*realloc)(void *, size_t),
	  void   (*free)(void *))
{
    em_state *state = NULL;

    state = (*alloc)(sizeof(em_state));
    if (!state)
	goto error;

    /* Stuff that might fail (used after the error label) */

    state->trace_state = NULL;

    /* Init state ... */

    state->alloc = alloc;
    state->realloc = realloc;
    state->free = free;

    state->block_table = NULL;
    state->carrier_table = NULL;

    reset_mem_info(&state->info.total);
    state->info.btype = NULL;
    state->info.allctr = NULL;

    state->info.allctr_prv_crr = NULL;
    state->info.allctr_usd_crr = NULL;

    state->info.stop_time.secs = 0;
    state->info.stop_time.usecs = 0;
    state->info.stop_reason = EMTP_UNDEF;
    state->info.exit_status = 0;

    state->output.next_print = 0;
    state->output.next_print_inc = 10;
    state->output.header = NULL;
    state->output.header_size = 0;
    state->output.values_per_object = 0;
    state->output.values_per_line = 0;
    state->output.field_width = 11;
    state->output.verbose = 0;
    state->output.total = 0;
    state->output.all_allctrs = 0;
    state->output.no_allctrs = 0;
    state->output.allctrs = NULL;
    state->output.all_btypes = 0;
    state->output.no_btypes = 0;
    state->output.btypes = NULL;
    state->output.max_min_values = 0;
    state->output.block_counts = 0;
    state->output.op_counts = 0;
    state->output.lines_until_header = EM_LINES_UNTIL_HEADER;

#if PRINT_OPERATIONS
    state->output.stream = stderr;
#else
    state->output.stream = stdout;
#endif
    state->output.file_name = NULL;
#if EMEM_d_SWITCH
    state->output.dir_name = NULL;
    state->output.erl_cmd_file = NULL;
    state->output.go.mutex = NULL;
    state->output.go.cond = NULL;
#endif

    init_queue(state, &state->output.queue);
    state->output.queue.max_buf_size = 10*1024*1024;
    state->output.queue.name = "output";

    state->trace_state = emtp_state_new(alloc, realloc, free);
    if (!state->trace_state)
	goto error;

    state->trace_info.version.parser.major = 0;
    state->trace_info.version.parser.minor = 0;
    state->trace_info.version.trace.major = 0;
    state->trace_info.version.trace.minor = 0;
    state->trace_info.bits = 0;
    state->trace_info.max_allocator_ix = 0;
    state->trace_info.allocator = NULL;
    state->trace_info.max_block_type_ix = 0;
    state->trace_info.block_type = NULL;

    state->input.listen_port = 0;
    state->input.socket = INVALID_SOCKET;
    state->input.total_trace_size = 0;
    state->input.error = 0;
    state->input.error_descr = NULL;

    init_queue(state, &state->input.queue);
    state->input.queue.max_buf_size = 10*1024*1024;
    state->input.queue.name = "input";

    return state;

 error:
    if (state) {
	if (state->trace_state)
	    emtp_state_destroy(state->trace_state);
	(*free)(state);
    }
    return NULL;
}


static emtbt_table *
mk_block_table(em_state *state)
{
    return emtbt_new_table(state->trace_info.bits == 64,
			   state->alloc,
			   state->realloc,
			   state->free);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 *                                                                         *
 *                                                                         *
\*                                                                         */
#if PRINT_OPERATIONS
void print_op(em_state *state, emtp_operation *op);
#endif

static INLINE void
update_max_values(em_mem_info *mi)
{
    if (mi->max_size < mi->size)
	mi->max_size = mi->size;
    if (mi->max_no < mi->no)
	mi->max_no = mi->no;
}

static INLINE void
update_min_values(em_mem_info *mi)
{
    if (mi->min_size > mi->size)
	mi->min_size = mi->size;
    if (mi->min_no > mi->no)
	mi->min_no = mi->no;
}

static INLINE void
update_alloc_op(em_mem_info *mi, usgnd_int_max size)
{
    mi->allocs++;
    mi->size += size;
    mi->no++;
    update_max_values(mi);
}

static INLINE void
update_realloc_op(em_mem_info *mi,
		  usgnd_int_max size,
		  usgnd_int_max prev_size,
		  int no_change)
{
    mi->reallocs++;
    ASSERT(mi->size >= prev_size);
    mi->size -= prev_size;
    mi->size += size;
    if (no_change) {
	if (no_change > 0)
	    mi->no++;
	else {
	    ASSERT(mi->no > 0);
	    mi->no--;
	}
    }
    update_max_values(mi);
    update_min_values(mi);
}

static INLINE void
update_free_op(em_mem_info *mi, usgnd_int_max prev_size)
{
    mi->frees++;
    ASSERT(mi->size >= prev_size);
    mi->size -= prev_size;
    ASSERT(mi->no > 0);
    mi->no--;
    update_min_values(mi);
}

static int
insert_operations(em_state *state, emtp_operation ops[], size_t len)
{
    emtbt_table *crr_table;
    emtbt_block old_blk;
    usgnd_int_32 prev_size;
    usgnd_int_max size;
    size_t i;
    int res;
    int aix, btix, crrix;

    for (i = 0; i < len; i++) {

	while (state->output.next_print <= ops[i].time.secs) {
	    print_info(state, state->output.next_print, NULL);
	    state->output.next_print += state->output.next_print_inc;
	}

	switch (ops[i].type) {

	case EMTP_ALLOC:
#if PRINT_OPERATIONS
	    print_op(state, &ops[i]);
#endif
	    btix = (int) ops[i].u.block.type;
	    aix = state->trace_info.block_type[btix]->allocator;

	    if (!ops[i].u.block.new_ptr)
		continue;

	    res = emtbt_alloc_op(state->block_table, &ops[i]);
	    if (res != 0)
		ERR_RET(res);

	    size = ops[i].u.block.new_size;

	    update_alloc_op(&state->info.btype[btix], size);
	    update_alloc_op(&state->info.allctr[aix], size);
	    update_alloc_op(&state->info.total, size);
	    break;

	case EMTP_REALLOC: {
	    int no;
#if PRINT_OPERATIONS
	    print_op(state, &ops[i]);
#endif

	    res = emtbt_realloc_op(state->block_table, &ops[i], &old_blk);
	    if (res != 0)
		ERR_RET(res);

	    size = ops[i].u.block.new_size;
	    prev_size = old_blk.size;
	    
	    if (!ops[i].u.block.prev_ptr)
		btix = (int) ops[i].u.block.type;
	    else
		btix = (int) old_blk.type;
	    aix = state->trace_info.block_type[btix]->allocator;
	    
	    no = ((!old_blk.pointer && ops[i].u.block.new_ptr)
		  ? 1
		  : ((old_blk.pointer && !ops[i].u.block.new_size)
		     ? -1
		     : 0));

	    update_realloc_op(&state->info.btype[btix], size, prev_size, no);
	    update_realloc_op(&state->info.allctr[aix], size, prev_size, no);
	    update_realloc_op(&state->info.total, size, prev_size, no);

	    break;
	}
	case EMTP_FREE:
#if PRINT_OPERATIONS
	    print_op(state, &ops[i]);
#endif

	    if (!ops[i].u.block.prev_ptr)
		continue;

	    res = emtbt_free_op(state->block_table, &ops[i], &old_blk);
	    if (res != 0)
		ERR_RET(res);

	    prev_size = old_blk.size;
	    btix = (int) old_blk.type;
	    aix = state->trace_info.block_type[btix]->allocator;


	    update_free_op(&state->info.btype[btix], prev_size);
	    update_free_op(&state->info.allctr[aix], prev_size);
	    update_free_op(&state->info.total, prev_size);

	    break;

	case EMTP_CARRIER_ALLOC:
#if PRINT_OPERATIONS
	    print_op(state, &ops[i]);
#endif

	    aix = (int) ops[i].u.block.type;

	    crrix = (int) ops[i].u.block.carrier_type;
	    if (!state->carrier_table[crrix]) {
		state->carrier_table[crrix] = mk_block_table(state);
		if (!state->carrier_table[crrix])
		    ERR_RET(ENOMEM);
	    }
	    crr_table = state->carrier_table[crrix];

	    if (!ops[i].u.block.new_ptr)
		continue;

	    res = emtbt_alloc_op(crr_table, &ops[i]);
	    if (res != 0)
		ERR_RET(res);

	    size = ops[i].u.block.new_size;

	    if (state->info.allctr_usd_crr[aix])
		update_alloc_op(state->info.allctr_usd_crr[aix], size);
	    if (state->info.allctr_prv_crr[crrix])
		update_alloc_op(state->info.allctr_prv_crr[crrix], size);
	    update_alloc_op(&state->info.allctr[crrix], size);

	    break;

	case EMTP_CARRIER_REALLOC: {
	    int no;
#if PRINT_OPERATIONS
	    print_op(state, &ops[i]);
#endif

	    crrix = (int) ops[i].u.block.carrier_type;
	    if (!state->carrier_table[crrix]) {
		state->carrier_table[crrix] = mk_block_table(state);
		if (!state->carrier_table[crrix])
		    ERR_RET(ENOMEM);
	    }
	    crr_table = state->carrier_table[crrix];

	    res = emtbt_realloc_op(crr_table, &ops[i], &old_blk);
	    if (res != 0)
		ERR_RET(res);

	    size = ops[i].u.block.new_size;
	    prev_size = old_blk.size;
	    
	    if (!ops[i].u.block.prev_ptr)
		aix = (int) ops[i].u.block.type;
	    else
		aix = (int) old_blk.type;


	    no = ((!old_blk.pointer && ops[i].u.block.new_ptr)
		  ? 1
		  : ((old_blk.pointer && !ops[i].u.block.new_size)
		     ? -1
		     : 0));

	    if (state->info.allctr_usd_crr[aix])
		update_realloc_op(state->info.allctr_usd_crr[aix],
				  size,
				  prev_size,
				  no);
	    if (state->info.allctr_prv_crr[crrix])
		update_realloc_op(state->info.allctr_prv_crr[crrix],
				  size,
				  prev_size,
				  no);
	    update_realloc_op(&state->info.allctr[crrix],
			      size,
			      prev_size,
			      no);
	    break;
	}
	case EMTP_CARRIER_FREE:
#if PRINT_OPERATIONS
	    print_op(state, &ops[i]);
#endif

	    crrix = (int) ops[i].u.block.carrier_type;
	    crr_table = state->carrier_table[crrix];
	    if (!crr_table)
		ERR_RET(EMTBT_FREE_NOBLK_ERROR);

	    if (!ops[i].u.block.prev_ptr)
		continue;

	    res = emtbt_free_op(crr_table, &ops[i], &old_blk);
	    if (res != 0)
		ERR_RET(res);

	    prev_size = old_blk.size;
	    aix = (int) old_blk.type;

	    if (state->info.allctr_usd_crr[aix])
		update_free_op(state->info.allctr_usd_crr[aix], prev_size);
	    if (state->info.allctr_prv_crr[crrix])
		update_free_op(state->info.allctr_prv_crr[crrix], prev_size);
	    update_free_op(&state->info.allctr[crrix], prev_size);

	    break;

	case EMTP_STOP:
#if PRINT_OPERATIONS
	    print_op(state, &ops[i]);
#endif
	    state->info.stop_reason = EMTP_STOP;
	    state->info.stop_time.secs = ops[i].time.secs;
	    state->info.stop_time.usecs = ops[i].time.usecs;
	    print_info(state, ops[i].time.secs, NULL);
	    return EM_EXIT_RESULT;
	case EMTP_EXIT:
#if PRINT_OPERATIONS
	    print_op(state, &ops[i]);
#endif
	    state->info.stop_reason = EMTP_EXIT;
	    state->info.exit_status = ops[i].u.exit_status;
	    state->info.stop_time.secs = ops[i].time.secs;
	    state->info.stop_time.usecs = ops[i].time.usecs;
	    print_info(state, ops[i].time.secs, NULL);
	    return EM_EXIT_RESULT;
	default:
#if PRINT_OPERATIONS
	    print_op(state, &ops[i]);
#endif
	    /* Ignore not understood operation */
	    break;
	}
    }
    return 0;
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 *                                                                         *
 *                                                                         *
\*                                                                         */

static const char *
error_string(int error)
{
    const char *str;
    const char *error_str;
    static const char unknown_error[] = "Unknown error";
    
    error_str = unknown_error;

    if (error > 0) {
	char *str = strerror(error);
	if (str)
	    error_str = str;
    }
    else if (error < 0) {
	str = emtp_error_string(error);
	if (!str) {
	    str = emtbt_error_string(error);
	    if (!str) {
		switch (error) {
		case EM_TRUNCATED_TRACE_ERROR:
		    error_str = "Truncated trace";
		    break;
		case EM_INTERNAL_ERROR:
		    error_str = "emem internal error";
		    break;
		default:
		    break;
		}
	    }
	}

	if (str)
	    error_str = str;
    }

    return error_str;
}

static void
error(int res)
{
    error_msg(res, NULL);
}

static void
error_msg(int res, char *msg)
{
    fprintf(stderr,
	    "emem: %s%sFatal error: %s (%d)\n",
	    msg ? msg : "",
	    msg ? ": ": "",
	    error_string(res),
	    res);
    exit(1);
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 *                                                                         *
 *                                                                         *
\*                                                                         */

#if EMEM_d_SWITCH

static size_t
write_output_filename(char *ptr,
		      char *dirname,
		      char *nodename,
		      char *hostname,
		      char *datetime,
		      char *pid)
{
    size_t sz = 0;
    char *p = ptr;
    char **pp = ptr ? &p : NULL;
    sz += write_str(pp, dirname);
    if (pp) *((*pp)++) = DIR_SEP_CHAR;
    sz++;
    sz += write_str(pp, nodename);
    sz += write_str(pp, "_");
    sz += write_str(pp, hostname);
    sz += write_str(pp, "_");
    sz += write_str(pp, datetime);
    sz += write_str(pp, "_");
    sz += write_str(pp, pid);
    sz += write_str(pp, EM_OUTPUT_FILE_SUFFIX);
    if (pp) *((*pp)++) = '\0';
    sz++;
    return sz;
}

static char *
make_output_filename(em_state *state)
{
    char *fname;
    size_t fname_size;
    char *nodename = state->trace_info.nodename;
    char *hostname = state->trace_info.hostname;
    char *pid = state->trace_info.pid;
    char dt_buf[20];
    char *date_time = NULL;

    if (*nodename == '\0')
	nodename = "nonode";
    if (*hostname == '\0')
	hostname = "nohost";
    if (!state->trace_info.start_time.day)
	date_time = "notime";
    else {
	sprintf(dt_buf,
		"%d-%2.2d-%2.2d_%2.2d.%2.2d.%2.2d",
		state->trace_info.start_time.year % 10000,
		state->trace_info.start_time.month % 100,
		state->trace_info.start_time.day % 100,
		state->trace_info.start_time.hour % 100,
		state->trace_info.start_time.minute % 100,
		state->trace_info.start_time.second % 100);
	date_time = &dt_buf[0];
    }
    if (*pid == '\0')
	pid = "nopid";

    fname = (*state->alloc)(write_output_filename(NULL,
						  state->output.dir_name,
						  nodename,
						  hostname,
						  date_time,
						  pid));
    if (!fname)
	return NULL;

    (void) write_output_filename(fname,
				 state->output.dir_name,
				 nodename,
				 hostname,
				 date_time,
				 pid);
    return fname;
}

#endif

static int
complete_state(em_state *state)
{
    int i, j, vpo, vpl;
    void * (*allocp)(size_t);
    void * (*reallocp)(void *, size_t);
    void (*freep)(void *);
    size_t size = sizeof(emtp_info);

    if (!emtp_get_info(&state->trace_info, &size, state->trace_state)
	|| size < sizeof(emtp_info))
	return EM_INTERNAL_ERROR;

#if EMEM_d_SWITCH

    if (!state->output.stream) {
	char *fname = make_filename(state);
	mutex_lock(state->output.go.mutex);
	state->output.stream = fopen(fname, "w");
	if (!state->output.stream) {
	    disconnect_queue_reader(&state->input.queue);
	    disconnect_queue_writer(&state->output.queue);
	}
	cond_signal(state->output.go.cond);
	mutex_unlock(state->output.go.mutex);
	(*state->free)((void *) fname);
	if (!state->output.stream)
	    return EIO;
    }

#endif

    allocp = state->alloc;
    reallocp = state->realloc;
    freep = state->free;


    state->carrier_table = (*allocp)((state->trace_info.max_allocator_ix+2)
				     * sizeof(emtbt_table *));
    if (!state->carrier_table)
	return ENOMEM;
    state->carrier_table++;
    for (i = -1; i <= state->trace_info.max_allocator_ix; i++)
	state->carrier_table[i] = NULL;


    state->block_table = mk_block_table(state);
    state->info.btype = (*allocp)((state->trace_info.max_block_type_ix+2)
				  * sizeof(em_mem_info));
    state->info.allctr = (*allocp)((state->trace_info.max_allocator_ix+2)
				   * sizeof(em_mem_info));
    if (!state->block_table || !state->info.btype || !state->info.allctr)
	return ENOMEM;

    state->info.btype++;
    state->info.allctr++;

    state->info.allctr_prv_crr
	= (*allocp)((state->trace_info.max_allocator_ix+2)
		    * sizeof(em_mem_info *));
    if (!state->info.allctr_prv_crr)
	return ENOMEM;
    state->info.allctr_prv_crr++;
    for (i = -1; i <= state->trace_info.max_allocator_ix; i++)
	state->info.allctr_prv_crr[i] = NULL;

    state->info.allctr_usd_crr
	= (*allocp)((state->trace_info.max_allocator_ix+2)
		    * sizeof(em_mem_info *));
    if (!state->info.allctr_usd_crr)
	return ENOMEM;
    state->info.allctr_usd_crr++;
    for (i = -1; i <= state->trace_info.max_allocator_ix; i++)
	state->info.allctr_usd_crr[i] = NULL;

    if (state->output.all_btypes) {
	if (state->output.btypes)
	    (*state->free)((void *) state->output.btypes);
	state->output.no_btypes = state->trace_info.max_block_type_ix + 2;
	state->output.btypes = (*allocp)(state->output.no_btypes
					 * sizeof(em_output_types));
	if (!state->output.btypes)
	    return ENOMEM;
    }

    if (state->output.all_allctrs) {
	if (state->output.allctrs)
	    (*state->free)((void *) state->output.allctrs);
	state->output.no_allctrs = state->trace_info.max_allocator_ix + 2;
	state->output.allctrs = (*allocp)(state->output.no_allctrs
					  * sizeof(em_output_types));
	if (!state->output.allctrs)
	    return ENOMEM;
    }

    for (i = -1; i <= state->trace_info.max_block_type_ix; i++) {
	/* Save block type if we should print info about it */
	emtp_block_type *btp = state->trace_info.block_type[i];
	reset_mem_info(&state->info.btype[i]);
	if (state->output.no_btypes) {
	    if (state->output.all_btypes) {
		state->output.btypes[i+1].name = btp->name;
		state->output.btypes[i+1].ix = btp->valid ? i : -1;
	    }
	    else {
		for (j = 0; j < state->output.no_btypes; j++)
		    if (strcmp(btp->name, state->output.btypes[j].name) == 0) {
			state->output.btypes[j].ix = i;
			break;
		    }
	    }
	}
    }

    /* Remove invalid block types */
    if (state->output.no_btypes) {
	for (i = 0, j = 0; i < state->output.no_btypes; i++) {
	    if (state->output.btypes[i].ix >= 0) {
		state->output.btypes[j].name = state->output.btypes[i].name;
		state->output.btypes[j].ix = state->output.btypes[i].ix;
		j++;
	    }
	}
	state->output.no_btypes = j;
    }

    for (i = -1; i <= state->trace_info.max_allocator_ix; i++) {
	/* Save allocator if we should print info about it */
	emtp_allocator *ap = state->trace_info.allocator[i];
	reset_mem_info(&state->info.allctr[i]);
	if (state->output.no_allctrs) {
	    if (state->output.all_allctrs) {
		state->output.allctrs[i+1].name = ap->name;
		state->output.allctrs[i+1].ix = ap->valid ? i : -1;
	    }
	    else {
		for (j = 0; j < state->output.no_allctrs; j++)
		    if (strcmp(ap->name, state->output.allctrs[j].name) == 0) {
			state->output.allctrs[j].ix = i;
			break;
		    }
	    }
	}

	/* Allocate em_mem_info if used carrier info is available */
	if (ap->flags & EMTP_ALLOCATOR_FLAG_HAVE_USED_CARRIERS_INFO
	    || (i == state->trace_info.segment_ix
		&& state->trace_info.have_segment_carrier_info)) {		
	    state->info.allctr_usd_crr[i]
		= (em_mem_info *) (*allocp)(sizeof(em_mem_info));
	    if (!state->info.allctr_usd_crr[i])
		return ENOMEM;
	    reset_mem_info(state->info.allctr_usd_crr[i]);
	}

	/* Allocate em_mem_info for carrier providers */
	if (ap->carrier.provider) {
	    sgnd_int_32 j;
	    for (j = 0; j < ap->carrier.no_providers; j++) {
		sgnd_int_32 crr_prvdr = ap->carrier.provider[j];
		if (!state->info.allctr_prv_crr[crr_prvdr]) {
		    state->info.allctr_prv_crr[crr_prvdr]
			= (em_mem_info *) (*allocp)(sizeof(em_mem_info));
		    if (!state->info.allctr_prv_crr[crr_prvdr])
			return ENOMEM;
		    reset_mem_info(state->info.allctr_prv_crr[crr_prvdr]);
		}
	    }
	}
    }

    /* Remove invalid allocators */
    if (state->output.no_allctrs) {
	for (i = 0, j = 0; i < state->output.no_allctrs; i++) {
	    if (state->output.allctrs[i].ix >= 0) {
		state->output.allctrs[j].name = state->output.allctrs[i].name;
		state->output.allctrs[j].ix = state->output.allctrs[i].ix;
		j++;
	    }
	}
	state->output.no_allctrs = j;
    }

    if (state->output.no_btypes) {
	state->output.btypes = (*reallocp)(state->output.btypes,
					   sizeof(em_output_types)
					   * state->output.no_btypes);
	if (!state->output.btypes)
	    return ENOMEM;
    }

    if (state->output.no_allctrs) {
	state->output.allctrs = (*reallocp)(state->output.allctrs,
					    sizeof(em_output_types)
					    * state->output.no_allctrs);
	if (!state->output.allctrs)
	    return ENOMEM;
    }


    vpo = 1;
    if (state->output.max_min_values)
	vpo += 2;
    if (state->output.block_counts) {
	vpo++;
	if (state->output.max_min_values)
	    vpo += 2;
    }
    if (state->output.op_counts)
	vpo += 3;

    state->output.values_per_object = vpo;

    vpl = 0;
    vpl++;					/* time */
    if (state->output.total) {
	vpl += vpo;				/* total allocated */
	if (state->trace_info.have_segment_carrier_info) {
	    vpl += vpo;				/* total carriers */
	    vpl += vpo;				/* cached carriers */
	}
    }
    for (i = 0; i < state->output.no_allctrs; i++) {
	vpl += vpo;				/* allocated */
	if (state->trace_info.have_carrier_info) {
	    if (state->info.allctr_prv_crr[state->output.allctrs[i].ix])
		vpl += vpo;			/* provided carriers */
	    vpl += vpo;				/* used carriers */
	}
    }
    vpl += state->output.no_btypes*vpo;		/* allocated */

    state->output.values_per_line = vpl;

    state->output.header_size = write_header(state, NULL, 1);
    state->output.header = (*state->alloc)(state->output.header_size + 1);
    if (!state->output.header)
	return ENOMEM;
    size = write_header(state, state->output.header, 1);
    ASSERT(state->output.header_size == size);
    return 0;
}

static int
process_trace(em_state *state)
{
    emtp_operation ops[EM_NO_OF_OPS];
    int res;
    size_t ops_len;
    em_area area;

    while (1) {
	get_next_read_area(&area, state, &state->input.queue);
	if (!area.size)
	    return EM_TRUNCATED_TRACE_ERROR;
	res = emtp_parse(state->trace_state,
			 (usgnd_int_8 **)&area.ptr, &area.size,
			 NULL, 0, NULL);
	if (res == EMTP_HEADER_PARSED)
	    break;
	if (res == EMTP_NEED_MORE_TRACE)
	    continue;

	if (res < 0)
	    return res;
	else
	    return EM_TRUNCATED_TRACE_ERROR;
    }
	
    res = complete_state(state);
    if (res != 0)
	return res;

    print_main_header(state);

    while (1) {
	if (!area.size) {
	    get_next_read_area(&area, state, &state->input.queue);
	    if (!area.size)
		return EM_TRUNCATED_TRACE_ERROR;

	}


	while (area.size) {
	    ops_len = EM_NO_OF_OPS;
	    res = emtp_parse(state->trace_state,
			     (usgnd_int_8 **)&area.ptr, &area.size,
			     ops, sizeof(emtp_operation), &ops_len);
	    if (res < 0)
		return res;

	    res = insert_operations(state, ops, ops_len);
	    if (res != 0)
		return res;

	}

    }

}

static void
usage(char *sw, char *error)
{
    int status = 0;
    FILE *filep = stdout;
#ifdef __WIN32__
#define SW_CHAR "/"
#else
#define SW_CHAR "-"
#endif

    if (error) {
	ASSERT(sw);
	status = 1;
	filep = stderr;
	fprintf(filep, "emem: %s: %s\n", sw, error);
    }
    fprintf(filep,
	    "Usage: emem "
#if EMEM_A_SWITCH
	    "[" SW_CHAR "A <ALLOCATOR>] "
#endif
	    "[" SW_CHAR "a <ALLOCATOR>] "
	    "[" SW_CHAR "b <BLOCK TYPE>] "
#if EMEM_C_SWITCH
	    "[" SW_CHAR "C <CLASS>] "
#endif
#if EMEM_c_SWITCH
	    "[" SW_CHAR "c <CLASS>] "
#endif
	    "{"
#if EMEM_d_SWITCH
	    SW_CHAR "d <DIRNAME>|"
#endif
	    SW_CHAR "f <FILENAME>} "
	    "[" SW_CHAR "h] "
	    "[" SW_CHAR "i <SECONDS>] "
	    "[" SW_CHAR "m] "
	    "[" SW_CHAR "n] "
	    "[" SW_CHAR "o] "
	    "{" SW_CHAR "p <PORT>} "
	    "[" SW_CHAR "t] "
	    "[" SW_CHAR "v] "
	    "\n");
    if (error)
	exit(1);
    else {
	fprintf(filep, 
	    "\n"
	    "  []        - switch is allowed any number of times\n"
	    "  {}        - switch is allowed at most one time\n"
#if EMEM_d_SWITCH
	    "  |         - exclusive or\n"
#endif
	    "\n"
	    " Switches:\n"
#if EMEM_A_SWITCH
	    "  " SW_CHAR "a <A>    - display info about Allocator <A> and all block types using <A>\n"
#endif
	    "  " SW_CHAR "a <A>    - display info about allocator <A>\n"
	    "  " SW_CHAR "b <B>    - display info about block type <B>\n"
#if EMEM_C_SWITCH
	    "  " SW_CHAR "C <C>    - display info about class <C> and all block types in class <C>\n"
#endif
#if EMEM_c_SWITCH
	    "  " SW_CHAR "b <B>    - display info about class <C>\n"
#endif
#if EMEM_d_SWITCH
	    "  " SW_CHAR "d <D>    - run as daemon and set output directory to <D>\n"
#endif
	    "  " SW_CHAR "f <F>    - set output file to <F>\n"
	    "  " SW_CHAR "h        - display help and exit\n"
	    "  " SW_CHAR "i <I>    - set display interval to <I> seconds\n"
	    "  " SW_CHAR "m        - display max/min values\n"
	    "  " SW_CHAR "n        - display block/carrier/segment count values\n"
	    "  " SW_CHAR "o        - display operation count values\n"
	    "  " SW_CHAR "p <P>    - set listen port to <P>\n"
	    "  " SW_CHAR "t        - display info about total values\n"
	    "  " SW_CHAR "v        - verbose output\n");
	exit(0);
    }

#undef SW_CHAR
}


static void
parse_args(em_state *state, int argc, char *argv[])
{
    int port;
    int i;

    port = -1;

    i = 1;
    while (i < argc) {
	if ((argv[i][0] != '-' && argv[i][0] != '/') || argv[i][2] != '\0') {
	unknown_switch:
	    usage(argv[i], "unknown switch");
	}

	switch (argv[i][1]) {
#if EMEM_A_SWITCH
	case 'A': /* TODO: Allocator + blocktypes using allocator */
#endif
	case 'a':
	    if (i + 1 >= argc)
		usage(argv[i], "missing allocator");
	    i++;
	    if (state->output.all_allctrs || strcmp(argv[i],"all") == 0) {
		state->output.all_allctrs = 1;
		break;
	    }

	    if (!state->output.allctrs)
		state->output.allctrs
		    = (*state->alloc)(sizeof(em_output_types)*argc/2);
	    if (!state->output.allctrs)
		error(ENOMEM);
	    state->output.allctrs[state->output.no_allctrs].name = argv[i];
	    state->output.allctrs[state->output.no_allctrs].ix = -1;
	    state->output.no_allctrs++;
	    break;
	case 'b':
	    if (i + 1 >= argc)
		usage(argv[i], "missing block type");
	    i++;
	    if (state->output.all_btypes || strcmp(argv[i],"all") == 0) {
		state->output.all_btypes = 1;
		break;
	    }

	    if (!state->output.btypes)
		state->output.btypes
		    = (*state->alloc)(sizeof(em_output_types)*argc/2);
	    if (!state->output.btypes)
		error(ENOMEM);
	    state->output.btypes[state->output.no_btypes].name = argv[i];
	    state->output.btypes[state->output.no_btypes].ix = -1;
	    state->output.no_btypes++;
	    break;
#if EMEM_C_SWITCH
#endif
#if EMEM_c_SWITCH
	case 'c':
	    break;
#endif
#if EMEM_d_SWITCH
	case 'd': {
	    char *p;
	    char *fname;
	    if (state->output.dir_name)
		usage(argv[i], "directory already set");
	    if (state->output.file_name)
		usage(argv[i], "file name already set");
	    if (i + 1 >= argc)
		usage(argv[i], "missing directory name");
	    state->output.dir_name = argv[i+1];
	    fname = (*state->alloc)(strlen(state->output.dir_name)
				    + 1
				    + strlen(EM_ERL_CMD_FILE_NAME)
				    + 1);
	    state->output.go.mutex = (*state->alloc)(sizeof(ethr_mutex));
	    state->output.go.cond = (*state->alloc)(sizeof(ethr_cond));
	    if (!fname || !state->output.go.mutex || !state->output.go.cond)
		error(ENOMEM);
	    p = fname;
	    (void) write_str(&p, state->output.dir_name);
	    *(p++) = DIR_SEP_CHAR;
	    (void) write_str(&p, EM_ERL_CMD_FILE_NAME);
	    *(p++) = '\0';
	    state->output.erl_cmd_file = fopen(fname, "w");
	    if (!state->output.erl_cmd_file)
		usage(argv[i], "cannot create files in directory");
	    (*state->free)((void *) fname);
	    state->output.stream = NULL;
	    mutex_init(state->output.go.mutex);
	    cond_init(state->output.go.cond);
	    i++;
	    break;
	}
#endif
	case 'f':
#if EMEM_d_SWITCH
	    if (state->output.dir_name)
		usage(argv[i], "directory already set");
#endif
	    if (state->output.file_name)
		usage(argv[i], "file name already set");
	    if (i + 1 >= argc)
		usage(argv[i], "missing file name");
	    state->output.file_name = argv[i+1];
	    state->output.stream = fopen(state->output.file_name, "w");
	    if (!state->output.stream)
		usage(argv[i], "cannot create file");
	    if (setvbuf(state->output.stream, NULL, _IONBF, 0) != 0) {
		fprintf(stderr,
			"emem: failed to set file %s in unbuffered mode\n",
			state->output.file_name);
		exit(1);
	    }
	    i++;
	    break;
	case 'h':
	    usage(NULL, NULL);
	    break;
	case 'i': {
	    int interval;
	    if (argv[i][2])
		goto unknown_switch;

	    if (i + 1 >= argc)
		usage(argv[i], "missing interval");
	    interval = atoi(argv[i+1]);
	    if (interval < 1) 
		usage(argv[i], "bad interval");
	    i++;
	    state->output.next_print_inc = interval;
	    break;
	}
	case 'm':
	    state->output.max_min_values = 1;
	    break;
	case 'n':
	    state->output.block_counts = 1;
	    break;
	case 'o':
	    state->output.op_counts = 1;
	    break;
	case 'p':
	    if (state->input.listen_port)
		usage(argv[i], "port already set");
	    if (i + 1 >= argc)
		usage(argv[i], "missing port number");
	    port = atoi(argv[i+1]);
	    if (port <= 1024 || port >= (1 << 16)) 
		usage(argv[i], "bad port number");
	    i++;
	    state->input.listen_port = (usgnd_int_16) port;
	    break;
	case 't':
	    state->output.total = 1;
	    break;
	case 'v':
	    state->output.verbose = 1;
	    break;
	default:
	    goto unknown_switch;
	}
	i++;
    }

    if (!state->output.allctrs && !state->output.btypes)
	state->output.total = 1;
}

static int
init_connection(em_state *state)
{
    int res;
    SOCKET lsock;
    SOCKET sock = INVALID_SOCKET;
    struct sockaddr_in my_addr;
    socklen_t oth_addr_len;
    struct sockaddr_in oth_addr;
#ifdef __WIN32__
    WORD wVersionRequested = MAKEWORD(2,0);
    WSADATA wsaData;

    if (WSAStartup(wVersionRequested, &wsaData) != 0)
	return EIO;

    if ((LOBYTE(wsaData.wVersion) != 2) || (HIBYTE(wsaData.wVersion) != 0))
	return EIO;
#endif

 do_socket:
    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (IS_INVALID_SOCKET(sock)) {
	res = GET_SOCK_ERRNO();
	if (res == EINTR)
	    goto do_socket;
	goto error;
    }

    memset((void *) &my_addr, 0, sizeof(struct sockaddr_in));

    my_addr.sin_family = AF_INET;
    my_addr.sin_addr.s_addr = htonl(INADDR_ANY);
    my_addr.sin_port = htons(state->input.listen_port);

 do_bind:
    if (bind(sock, (struct sockaddr*) &my_addr, sizeof(my_addr)) < 0) {
	res = GET_SOCK_ERRNO();
	if (res == EINTR)
	    goto do_bind;
	goto error;
    }

 do_listen:
    if (listen(sock, 1) < 0) {
	res = GET_SOCK_ERRNO();
	if (res == EINTR)
	    goto do_listen;
	goto error;
    }

    lsock = sock;
    state->input.socket = sock;

    res = print_emu_arg(state);
    if (res != 0)
	goto error;

    print_string(state, "> Waiting for emulator to connect... ");

 do_accept:
    oth_addr_len = sizeof(oth_addr);
    sock = accept(lsock, (struct sockaddr *) &oth_addr, &oth_addr_len);
    if (IS_INVALID_SOCKET(sock)) {
	res = GET_SOCK_ERRNO();
	if (res == EINTR)
	    goto do_accept;
	sock = lsock;
	goto error;
    }

    print_string(state, "connected\n");

    closesocket(lsock);
    state->input.socket = sock;

    return 0;

 error:
    if (!IS_INVALID_SOCKET(sock)) {
	closesocket(sock);
	state->input.socket = INVALID_SOCKET;
    }
    return res;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * IO threads                                                              *
 *                                                                         *
\*                                                                         */

/*
 * The input thread reads from a socket and puts the received data
 * in the input buffer queue.
 *
 * Note: There is intentionally no flow control. If the emem program
 *       cannot process data as fast as it arrives, it is supposed
 *       to crash when hitting the maximum buffersize; otherwise,
 *       the traced emulator would be slowed down.
 */
static void *
input_thread_func(void *arg)
{
    int res;
    char *edescr = NULL;
    ssize_t recv_sz;
    usgnd_int_max total_trace_size = 0;
    em_state *state = (em_state *) arg;
    em_area area = {NULL, 0};
    SOCKET sock = state->input.socket;
    em_buf_queue *queue = &state->input.queue;

    while(1) {
	get_next_write_area(&area,
			    state,
			    queue,
			    EM_MIN_TRACE_READ_SIZE);

	if (!area.ptr) {
	    res = ENOMEM;
	    edescr = "Input alloc";
	    goto stop;
	}

    do_recv:
	if (is_queue_reader_disconnected(queue)) {
	    res = 0;
	    edescr = "Input";
	    goto stop;
	}
	recv_sz = recv(sock, (void *) area.ptr, area.size, 0);
	if (recv_sz <= 0) {
	    res = GET_SOCK_ERRNO();
	    if (res == EINTR)
		goto do_recv;
	    edescr = "Input recv";
	    goto stop;
	}
	area.size = (size_t) recv_sz;
	total_trace_size += (usgnd_int_max) recv_sz;
    }

 stop:
    state->input.error = res;
    state->input.error_descr = edescr;
    state->input.total_trace_size = total_trace_size;
    disconnect_queue_writer(queue);
    if (!IS_INVALID_SOCKET(state->input.socket)) {
	closesocket(sock);
	state->input.socket = INVALID_SOCKET;
    }
    return NULL;
}


static void *
output_thread_func(void *arg)
{
    em_state *state = (em_state *) arg;
    em_area area = {NULL, 0};

#if EMEM_d_SWITCH

    if (state->output.go.mutex) {
	mutex_lock(state->output.go.mutex);
	while (!state->output.stream
	       && !is_queue_writer_disconnected(&state->output.queue))
	    cond_wait(state->output.go.cond, state->output.go.mutex);
	mutex_unlock(state->output.go.mutex);

	mutex_destroy(state->output.go.mutex);
	(*state->free)((void *) state->output.go.mutex);
	state->output.go.mutex = NULL;
	cond_destroy(state->output.go.cond);
	(*state->free)((void *) state->output.go.cond);
	state->output.go.cond = NULL;
    }

#endif

    while (1) {
	get_next_read_area(&area, state, &state->output.queue);
	if (!area.size) {
	    disconnect_queue_reader(&state->output.queue);
	    if (is_queue_writer_disconnected(&state->output.queue))
		goto stop;
	    else
		error_msg(EIO, "Output queue");
	}
	if (fwrite((void *) area.ptr,
		   sizeof(char),
		   area.size,
		   state->output.stream) != area.size) {
	    disconnect_queue_reader(&state->output.queue);
	    error_msg(0, "Write");
	}
    }

 stop:
    if (state->output.stream != stdout && state->output.stream != stderr)
	fclose(state->output.stream);
    return NULL;
}


int
main(int argc, char *argv[])
{
    int res, ires, jres;
    ethr_tid input_tid;
    ethr_tid output_tid;
    em_state *state;

    /* set stdout in unbuffered mode */
    if (setvbuf(stdout, NULL, _IONBF, 0) != 0) {
	fprintf(stderr, "emem: failed to set stdout in unbuffered mode\n");
	exit(1);
    }

    if (ethr_init(NULL) != 0 || ethr_late_init(NULL) != 0) {
	fprintf(stderr, "emem: failed to initialize thread package\n");
	exit(1);
    }
	
    state = new_state(malloc, realloc, free);
    if (!state)
	error(ENOMEM);

    parse_args(state, argc, argv);

    res = ethr_thr_create(&output_tid,
			  output_thread_func,
			  (void *) state,
			  NULL);
    if (res != 0)
	error_msg(res, "Output thread create");

#ifdef DEBUG
    print_string(state, "> [debug]\n");
#endif
#ifdef PURIFY
    print_string(state, "> [purify]\n");
#endif
#ifdef QUANTIFY
    print_string(state, "> [quantify]\n");
#endif
#ifdef PURECOV
    print_string(state, "> [purecov]\n");
#endif

    res = init_connection(state);
    if (res != 0)
	error_msg(res, "Initialize connection");

    res = ethr_thr_create(&input_tid,
			  input_thread_func,
			  (void *) state,
			  NULL);
    if (res != 0)
	error_msg(res, "Input thread create");

    res = process_trace(state);

    disconnect_queue_reader(&state->input.queue);

    jres = ethr_thr_join(input_tid, NULL);
    if (jres != 0)
	error_msg(jres, "Input thread join");

    if (res == EM_EXIT_RESULT)
	print_main_footer(state);
    disconnect_queue_writer(&state->output.queue);

    jres = ethr_thr_join(output_tid, NULL);
    if (jres != 0)
	error_msg(jres, "Output thread join");

    ires = state->input.error;

    destroy_state(state);

#ifdef __WIN32__
    WSACleanup();
#endif

    switch (res) {
    case EM_EXIT_RESULT:
	res = 0;
	break;
    case EM_TRUNCATED_TRACE_ERROR:
	error_msg(ires, state->input.error_descr);
	break;
    default:
	error(res);
	break;
    }

    return 0;
}


#if PRINT_OPERATIONS
void
print_op(em_state *state, emtp_operation *op)
{

#if 0
    printf("%5" USGND_INT_32_FSTR ":%6.6" USGND_INT_32_FSTR " ",
	   op->time.secs, op->time.usecs);
#endif
    if (state->trace_info.version.parser.major >= 2) {

	switch (op->type) {
	case EMTP_ALLOC:
	    printf(" %" USGND_INT_MAX_FSTR " = alloc(%" USGND_INT_16_FSTR
		   ", %" USGND_INT_MAX_FSTR ")\n",
		   op->u.block.new_ptr,
		   op->u.block.type,
		   op->u.block.new_size);
	    break;
	case EMTP_REALLOC:
	    printf(" %" USGND_INT_MAX_FSTR " = realloc(%" USGND_INT_16_FSTR
		   ", %" USGND_INT_MAX_FSTR ", %" USGND_INT_MAX_FSTR ")\n",
		   op->u.block.new_ptr,
		   op->u.block.type,
		   op->u.block.prev_ptr,
		   op->u.block.new_size);
	    break;
	case EMTP_FREE:
	    printf(" free(%" USGND_INT_16_FSTR ", %" USGND_INT_MAX_FSTR ")"
		   "\n",
		   op->u.block.type,
		   op->u.block.prev_ptr);
	    break;
	case EMTP_CARRIER_ALLOC:
	    printf(" %" USGND_INT_MAX_FSTR " = carrier_alloc(%"
		   USGND_INT_16_FSTR ", %" USGND_INT_16_FSTR ", %"
		   USGND_INT_MAX_FSTR ")\n",
		   op->u.block.new_ptr,
		   op->u.block.carrier_type,
		   op->u.block.type,
		   op->u.block.new_size);
	    break;
	case EMTP_CARRIER_REALLOC:
	    printf(" %" USGND_INT_MAX_FSTR " = carrier_realloc(%"
		   USGND_INT_16_FSTR ", %" USGND_INT_16_FSTR ", %"
		   USGND_INT_MAX_FSTR ", %" USGND_INT_MAX_FSTR ")\n",
		   op->u.block.new_ptr,
		   op->u.block.carrier_type,
		   op->u.block.type,
		   op->u.block.prev_ptr,
		   op->u.block.new_size);
	case EMTP_CARRIER_FREE:
	    printf(" carrier_free(%" USGND_INT_16_FSTR ", %" USGND_INT_16_FSTR
		   ", %" USGND_INT_MAX_FSTR ")\n",
		   op->u.block.carrier_type,
		   op->u.block.type,
		   op->u.block.prev_ptr);
	    break;
	default:
	    printf(" op = %d\n", op->type);
	    break;
	}

    }
    else {

	switch (op->type) {
	case EMTP_ALLOC:
	    printf(" %" USGND_INT_MAX_FSTR " = alloc(%" USGND_INT_MAX_FSTR ")"
		   "\n",
		   op->u.block.new_ptr,
		   op->u.block.new_size);
	    break;
	case EMTP_REALLOC:
	    printf(" %" USGND_INT_MAX_FSTR " = realloc(%" USGND_INT_MAX_FSTR
		   ", %" USGND_INT_MAX_FSTR ")\n",
		   op->u.block.new_ptr,
		   op->u.block.prev_ptr,
		   op->u.block.new_size);
	    break;
	case EMTP_FREE:
	    printf(" free(%" USGND_INT_MAX_FSTR ")\n",
		   op->u.block.prev_ptr);
	    break;
	default:
	    printf(" op = %d\n", op->type);
	    break;
	}
    }
    fflush(stdout);
}
#endif
