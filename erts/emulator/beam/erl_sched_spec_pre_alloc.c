/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2011-2018. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif


#include "erl_process.h"
#include "erl_thr_progress.h"

erts_sspa_data_t *
erts_sspa_create(size_t blk_sz, int pa_size, int nthreads, const char* name)
{
    erts_sspa_data_t *data;
    size_t tot_size;
    size_t chunk_mem_size;
    char *p;
    char *chunk_start;
    int cix;
    int no_blocks = pa_size;
    int no_blocks_per_chunk;
    size_t aligned_blk_sz;

#if !defined(ERTS_STRUCTURE_ALIGNED_ALLOC)
    /* Force 64-bit alignment... */
    aligned_blk_sz = ((blk_sz - 1) / 8) * 8 + 8;
#else
    /* Alignment of structure is enough... */
    aligned_blk_sz = blk_sz;
#endif

    if (!name) { /* schedulers only variant */
        ASSERT(!nthreads);
        nthreads = erts_no_schedulers;
    }
    else {
        ASSERT(nthreads > 0);
    }

    if (nthreads == 1)
	no_blocks_per_chunk = no_blocks;
    else {
	int extra = (no_blocks - 1)/4 + 1;
	if (extra == 0)
	    extra = 1;
	no_blocks_per_chunk = no_blocks;
	no_blocks_per_chunk += extra * nthreads;
	no_blocks_per_chunk /= nthreads;
    }
    no_blocks = no_blocks_per_chunk * nthreads;
    chunk_mem_size = ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(erts_sspa_chunk_header_t));
    chunk_mem_size += aligned_blk_sz * no_blocks_per_chunk;
    chunk_mem_size = ERTS_ALC_CACHE_LINE_ALIGN_SIZE(chunk_mem_size);
    tot_size = ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(erts_sspa_data_t));
    tot_size += chunk_mem_size * nthreads;

    p = erts_alloc_permanent_cache_aligned(ERTS_ALC_T_PRE_ALLOC_DATA, tot_size);
    data = (erts_sspa_data_t *) p;
    p += ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(erts_sspa_data_t));
    chunk_start = p;

    data->chunks_mem_size = chunk_mem_size;
    data->start = chunk_start;
    data->end = chunk_start + chunk_mem_size * nthreads;
    data->nthreads = nthreads;

    if (name) { /* thread variant */
        erts_tsd_key_create(&data->tsd_key, (char*)name);
        erts_atomic_init_nob(&data->id_generator, 0);
    }

    /* Initialize all chunks */
    for (cix = 0; cix < nthreads; cix++) {
	erts_sspa_chunk_t *chnk = erts_sspa_cix2chunk(data, cix);
	erts_sspa_chunk_header_t *chdr = &chnk->aligned.header;
	erts_sspa_blk_t *blk;
	int i;

	erts_atomic_init_nob(&chdr->tail.data.last, (erts_aint_t) &chdr->tail.data.marker);
	erts_atomic_init_nob(&chdr->tail.data.marker.next_atmc, ERTS_AINT_NULL);
	erts_atomic_init_nob(&chdr->tail.data.um_refc[0], 0);
	erts_atomic_init_nob(&chdr->tail.data.um_refc[1], 0);
	erts_atomic32_init_nob(&chdr->tail.data.um_refc_ix, 0);

	chdr->head.no_thr_progress_check = 0;
	chdr->head.used_marker = 1;
	chdr->head.first = &chdr->tail.data.marker;
	chdr->head.unref_end = &chdr->tail.data.marker;
	chdr->head.next.thr_progress = erts_thr_progress_current();
	chdr->head.next.thr_progress_reached = 1;
	chdr->head.next.um_refc_ix = 1;
	chdr->head.next.unref_end = &chdr->tail.data.marker;

	p = &chnk->data[0];
	chdr->local.first = (erts_sspa_blk_t *) p;
	blk = (erts_sspa_blk_t *) p;
	for (i = 0; i < no_blocks_per_chunk; i++) {
	    blk = (erts_sspa_blk_t *) p;
	    p += aligned_blk_sz;
	    blk->next_ptr = (erts_sspa_blk_t *) p;
	}

	blk->next_ptr = NULL;
	chdr->local.last = blk;
	chdr->local.cnt = no_blocks_per_chunk;
	chdr->local.lim = no_blocks_per_chunk / 3;

	ERTS_SSPA_DBG_CHK_LCL(chdr);
    }

    return data;
}

static ERTS_INLINE void
enqueue_remote_managed_thread(erts_sspa_chunk_header_t *chdr,
			      erts_sspa_blk_t *this,
			      int cinit)
{
    erts_aint_t itmp;
    erts_sspa_blk_t *enq;

    erts_atomic_init_nob(&this->next_atmc, ERTS_AINT_NULL);
    /* Enqueue at end of list... */

    enq = (erts_sspa_blk_t *) erts_atomic_read_nob(&chdr->tail.data.last);
    itmp = erts_atomic_cmpxchg_relb(&enq->next_atmc,
				    (erts_aint_t) this,
				    ERTS_AINT_NULL);
    if (itmp == ERTS_AINT_NULL) {
	/* We are required to move last pointer */
#ifdef DEBUG
	ASSERT(ERTS_AINT_NULL == erts_atomic_read_nob(&this->next_atmc));
	ASSERT(((erts_aint_t) enq)
	       == erts_atomic_xchg_relb(&chdr->tail.data.last,
				      (erts_aint_t) this));
#else
	erts_atomic_set_relb(&chdr->tail.data.last, (erts_aint_t) this);
#endif
    }
    else {
	/*
	 * We *need* to insert element somewhere in between the
	 * last element we read earlier and the actual last element.
	 */
	int i = cinit;

	while (1) {
	    erts_aint_t itmp2;
	    erts_atomic_set_nob(&this->next_atmc, itmp);
	    itmp2 = erts_atomic_cmpxchg_relb(&enq->next_atmc,
					     (erts_aint_t) this,
					     itmp);
	    if (itmp == itmp2)
		break; /* inserted this */
	    if ((i & 1) == 0)
		itmp = itmp2;
	    else {
		enq = (erts_sspa_blk_t *) itmp2;
		itmp = erts_atomic_read_acqb(&enq->next_atmc);
		ASSERT(itmp != ERTS_AINT_NULL);
	    }
	    i++;
	}
    }
}

static ERTS_INLINE erts_aint_t
check_insert_marker(erts_sspa_chunk_header_t *chdr, erts_aint_t ilast)
{
    if (!chdr->head.used_marker
	&& chdr->head.unref_end == (erts_sspa_blk_t *) ilast) {
	erts_aint_t itmp;
	erts_sspa_blk_t *last = (erts_sspa_blk_t *) ilast;

	erts_atomic_init_nob(&chdr->tail.data.marker.next_atmc, ERTS_AINT_NULL);
	itmp = erts_atomic_cmpxchg_relb(&last->next_atmc,
					(erts_aint_t) &chdr->tail.data.marker,
					ERTS_AINT_NULL);
	if (itmp == ERTS_AINT_NULL) {
	    ilast = (erts_aint_t) &chdr->tail.data.marker;
	    chdr->head.used_marker = !0;
	    erts_atomic_set_relb(&chdr->tail.data.last, ilast);
	}
    }
    return ilast;
}

void
erts_sspa_remote_free(erts_sspa_chunk_header_t *chdr,
		      erts_sspa_blk_t *blk,
		      int cinit)
{
    int um_refc_ix = 0;
    int managed_thread = erts_thr_progress_is_managed_thread();
    if (!managed_thread) {
	um_refc_ix = erts_atomic32_read_acqb(&chdr->tail.data.um_refc_ix);
	while (1) {
	    int tmp_um_refc_ix;
	    erts_atomic_inc_acqb(&chdr->tail.data.um_refc[um_refc_ix]);
	    tmp_um_refc_ix = erts_atomic32_read_acqb(&chdr->tail.data.um_refc_ix);
	    if (tmp_um_refc_ix == um_refc_ix)
		break;
	    erts_atomic_dec_relb(&chdr->tail.data.um_refc[um_refc_ix]);
	    um_refc_ix = tmp_um_refc_ix;
	}
    }

    enqueue_remote_managed_thread(chdr, blk, cinit);

    if (!managed_thread)
	erts_atomic_dec_relb(&chdr->tail.data.um_refc[um_refc_ix]);
}

static ERTS_INLINE void
fetch_remote(erts_sspa_chunk_header_t *chdr, int max)
{
    int new_local = 0;

    if (chdr->head.no_thr_progress_check < ERTS_SSPA_FORCE_THR_CHECK_PROGRESS)
	chdr->head.no_thr_progress_check++;
    else {
	erts_aint_t ilast;

	chdr->head.no_thr_progress_check = 0;

	ilast = erts_atomic_read_nob(&chdr->tail.data.last);
	if (((erts_sspa_blk_t *) ilast) == &chdr->tail.data.marker
	    && chdr->head.first == &chdr->tail.data.marker)
	    return;

	if (chdr->head.next.thr_progress_reached
	    || erts_thr_progress_has_reached(chdr->head.next.thr_progress)) {
	    int um_refc_ix;
	    chdr->head.next.thr_progress_reached = 1;
	    um_refc_ix = chdr->head.next.um_refc_ix;
	    if (erts_atomic_read_nob(&chdr->tail.data.um_refc[um_refc_ix]) == 0) {

		ETHR_MEMBAR(ETHR_LoadLoad|ETHR_LoadStore);

		/* Move unreferenced end pointer forward... */

		chdr->head.unref_end = chdr->head.next.unref_end;

		ilast = check_insert_marker(chdr, ilast);

		if (chdr->head.unref_end != (erts_sspa_blk_t *) ilast) {
		    chdr->head.next.unref_end = (erts_sspa_blk_t *) ilast;
		    chdr->head.next.thr_progress = erts_thr_progress_later(NULL);
		    erts_atomic32_set_relb(&chdr->tail.data.um_refc_ix,
					   um_refc_ix);
		    chdr->head.next.um_refc_ix = um_refc_ix == 0 ? 1 : 0;
		    chdr->head.next.thr_progress_reached = 0;
		}
	    }
	}
    }

    if (new_local < max && chdr->head.first != chdr->head.unref_end) {
	erts_sspa_blk_t *first, *this, *next, *last;
	first = chdr->head.first;
	if (first == &chdr->tail.data.marker) {
	    chdr->head.used_marker = 0;
	    first = ((erts_sspa_blk_t *)
		     erts_atomic_read_nob(&first->next_atmc));
	    chdr->head.first = first;
	}
	if (first != chdr->head.unref_end) {

	    ERTS_SSPA_DBG_CHK_LCL(chdr);

	    this = last = first;
	    do {
		next = (erts_sspa_blk_t *) erts_atomic_read_nob(&this->next_atmc);
		if (this == &chdr->tail.data.marker)
		    chdr->head.used_marker = 0;
		else {
		    last->next_ptr = this;
		    last = this;
		    new_local++;
		}
		this = next;
	    } while (new_local < max && this != chdr->head.unref_end);
	    chdr->head.first = this;
	    if (!chdr->local.last)
		chdr->local.first = first;
	    else
		chdr->local.last->next_ptr = first;
	    chdr->local.last = last;
	    last->next_ptr = NULL;
	    chdr->local.cnt += new_local;

	    ERTS_SSPA_DBG_CHK_LCL(chdr);
	}
    }

}

erts_sspa_blk_t *
erts_sspa_process_remote_frees(erts_sspa_chunk_header_t *chdr,
			       erts_sspa_blk_t *old_res)
{
    erts_sspa_blk_t *res = old_res;

    fetch_remote(chdr, ERTS_SSPA_MAX_GET_NEW_LOCAL);

    if (!res && chdr->local.first) {

	ERTS_SSPA_DBG_CHK_LCL(chdr);

	res = chdr->local.first;
	chdr->local.first = res->next_ptr;
	chdr->local.cnt--;
	if (!chdr->local.first)
	    chdr->local.last = NULL;

	ERTS_SSPA_DBG_CHK_LCL(chdr);
    }

    return res;
}

