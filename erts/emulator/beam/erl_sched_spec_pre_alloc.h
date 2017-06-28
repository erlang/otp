/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2011-2016. All Rights Reserved.
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
 * Description: Scheduler specific pre-allocators. Each scheduler
 *              thread allocates memory in its own private chunk of
 *              memory. Memory blocks deallocated by remote
 *              schedulers (or other threads) are passed back to
 *              the chunk owner via a lock-free data structure.
 *
 * Author: 	Rickard Green
 */

#ifndef ERTS_SCHED_SPEC_PRE_ALLOC_H__
#define ERTS_SCHED_SPEC_PRE_ALLOC_H__


#undef ERL_THR_PROGRESS_TSD_TYPE_ONLY
#define ERL_THR_PROGRESS_TSD_TYPE_ONLY
#include "erl_thr_progress.h"
#undef ERL_THR_PROGRESS_TSD_TYPE_ONLY

#ifdef DEBUG
#define ERTS_SPPA_DBG_CHK_IN_CHNK(A, C, P)				\
do {									\
    ASSERT((void *) (C) < (void *) (P));				\
    ASSERT((void *) (P)							\
	   < (void *) (((char *) (C)) + (A)->chunks_mem_size));		\
} while (0)
#else
#define ERTS_SPPA_DBG_CHK_IN_CHNK(A, C, P)
#endif

#ifdef DEBUG
extern Uint erts_no_schedulers;
#endif

#define ERTS_SSPA_FORCE_THR_CHECK_PROGRESS 10
#define ERTS_SSPA_MAX_GET_NEW_LOCAL 5

typedef struct {
    char *start;
    char *end;
    int chunks_mem_size;
    int nthreads;

    /* Used only by thread variant: */
    erts_tsd_key_t tsd_key;
    erts_atomic_t id_generator;
} erts_sspa_data_t;

typedef union erts_sspa_blk_t_ erts_sspa_blk_t;
union erts_sspa_blk_t_ {
    erts_atomic_t next_atmc;
    erts_sspa_blk_t *next_ptr;
};

typedef struct {
    erts_sspa_blk_t *first;
    erts_sspa_blk_t *last;
    int cnt;
    int lim;
} erts_sspa_local_freelist_t;

typedef struct {
    erts_sspa_blk_t marker;
    erts_atomic_t last;
    erts_atomic_t um_refc[2];
    erts_atomic32_t um_refc_ix;
} erts_sspa_tail_t;

typedef struct {
    /*
     * This structure needs to be cache line aligned for best
     * performance.
     */
    union {
	/* Modified by threads returning memory to this chunk */
	erts_sspa_tail_t data;
	char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(erts_sspa_tail_t))];
    } tail;
    /*
     * Everything below this point is *only* accessed by the
     * thread owning this chunk.
     */
    struct {
	int no_thr_progress_check;
	int used_marker;
	erts_sspa_blk_t *first;
	erts_sspa_blk_t *unref_end;
	struct {
	    ErtsThrPrgrVal thr_progress;
	    int thr_progress_reached;
	    int um_refc_ix;
	    erts_sspa_blk_t *unref_end;
	} next;
    } head;
    erts_sspa_local_freelist_t local;
} erts_sspa_chunk_header_t;

typedef struct {
    union {
	erts_sspa_chunk_header_t header;
	char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(
		sizeof(erts_sspa_chunk_header_t))];
    } aligned;
    char data[1];
} erts_sspa_chunk_t;

#ifdef DEBUG
ERTS_GLB_INLINE void
check_local_list(erts_sspa_chunk_header_t *chdr);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE void
check_local_list(erts_sspa_chunk_header_t *chdr)
{
    erts_sspa_blk_t *blk;
    int n = 0;
    for (blk = chdr->local.first; blk; blk = blk->next_ptr)
	n++;
    ASSERT(n == chdr->local.cnt);
}
#endif
#define ERTS_SSPA_DBG_CHK_LCL(CHDR) check_local_list((CHDR))
#else
#define ERTS_SSPA_DBG_CHK_LCL(CHDR)
#endif

erts_sspa_data_t *erts_sspa_create(size_t blk_sz,
				   int pa_size,
                                   int nthreads,
                                   const char* name);
void erts_sspa_remote_free(erts_sspa_chunk_header_t *chdr,
			   erts_sspa_blk_t *blk,
			   int cinit);
erts_sspa_blk_t *erts_sspa_process_remote_frees(erts_sspa_chunk_header_t *chdr,
						erts_sspa_blk_t *old_res);

ERTS_GLB_INLINE erts_sspa_chunk_t *erts_sspa_cix2chunk(erts_sspa_data_t *data,
						       int cix);
ERTS_GLB_INLINE int erts_sspa_ptr2cix(erts_sspa_data_t *data, void *ptr);
ERTS_GLB_INLINE char *erts_sspa_alloc(erts_sspa_data_t *data, int cix);
ERTS_GLB_INLINE int erts_sspa_free(erts_sspa_data_t *data, int cix, char *blk);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE erts_sspa_chunk_t *
erts_sspa_cix2chunk(erts_sspa_data_t *data, int cix)
{
    ASSERT(0 <= cix && cix < data->nthreads);
    return (erts_sspa_chunk_t *) (data->start + cix*data->chunks_mem_size);
}

ERTS_GLB_INLINE int
erts_sspa_ptr2cix(erts_sspa_data_t *data, void *ptr)
{
    int cix;
    size_t diff;
    if ((char *) ptr < data->start || data->end <= (char *) ptr)
	return -1;
    diff = ((char *) ptr) - data->start;
    cix = (int) diff / data->chunks_mem_size;
    ASSERT(0 <= cix && cix < data->nthreads);
    return cix;
}

ERTS_GLB_INLINE char *
erts_sspa_alloc(erts_sspa_data_t *data, int cix)
{
    erts_sspa_chunk_t *chnk;
    erts_sspa_chunk_header_t *chdr;
    erts_sspa_blk_t *res;

    chnk = erts_sspa_cix2chunk(data, cix);
    chdr = &chnk->aligned.header;
    res = chdr->local.first;
    ERTS_SSPA_DBG_CHK_LCL(chdr);
    if (res) {
	ERTS_SSPA_DBG_CHK_LCL(chdr);
	chdr->local.first = res->next_ptr;
	chdr->local.cnt--;
	if (!chdr->local.first)
	    chdr->local.last = NULL;
	ERTS_SSPA_DBG_CHK_LCL(chdr);
    }
    if (chdr->local.cnt <= chdr->local.lim)
	return (char *) erts_sspa_process_remote_frees(chdr, res);
    else if (chdr->head.no_thr_progress_check < ERTS_SSPA_FORCE_THR_CHECK_PROGRESS)
	chdr->head.no_thr_progress_check++;
    ASSERT(res);
    return (char *) res;
}

ERTS_GLB_INLINE int
erts_sspa_free(erts_sspa_data_t *data, int cix, char *cblk)
{
    erts_sspa_chunk_t *chnk;
    erts_sspa_chunk_header_t *chdr;
    erts_sspa_blk_t *blk = (erts_sspa_blk_t *) cblk;
    int chnk_cix = erts_sspa_ptr2cix(data, blk);

    if (chnk_cix < 0)
	return 0;

    chnk = erts_sspa_cix2chunk(data, chnk_cix);
    chdr = &chnk->aligned.header;
    if (chnk_cix != cix) {
	/* Remote chunk */
	erts_sspa_remote_free(chdr, blk, chnk_cix - cix);
    }
    else {
	/* Local chunk */
	ERTS_SSPA_DBG_CHK_LCL(chdr);
	blk->next_ptr = chdr->local.first;
	chdr->local.first = blk;
	if (!chdr->local.last)
	    chdr->local.last = blk;
	chdr->local.cnt++;
	ERTS_SSPA_DBG_CHK_LCL(chdr);
    }

    return 1;
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */


#endif /* ERTS_SCHED_SPEC_PRE_ALLOC_H__ */
