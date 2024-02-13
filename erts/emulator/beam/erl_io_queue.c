/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2017-2024. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "global.h"

#include "bif.h"

#include "erl_bits.h"
#include "erl_io_queue.h"

#define IOL2V_ACC_SIZE (ERL_ONHEAP_BINARY_LIMIT * 4)
#define IOL2V_BYTES_PER_REDUCTION (16)

static void free_binary(ErtsIOQBinary *b, int driver);
static ErtsIOQBinary *alloc_binary(Uint size, char *source, void **iov_base, int driver);

void erts_ioq_init(ErtsIOQueue *q, ErtsAlcType_t alct, int driver)
{

    ERTS_CT_ASSERT(offsetof(ErlNifIOVec,flags) == sizeof(ErtsIOVecCommon));
    ERTS_CT_ASSERT(sizeof(ErlIOVec) == sizeof(ErtsIOVecCommon));
    ERTS_CT_ASSERT(sizeof(size_t) == sizeof(ErlDrvSizeT));
    ERTS_CT_ASSERT(sizeof(size_t) == sizeof(Uint));

    q->alct = alct;
    q->driver = driver;
    q->size = 0;
    q->v_head = q->v_tail = q->v_start = q->v_small;
    q->v_end = q->v_small + ERTS_SMALL_IO_QUEUE;
    q->b_head = q->b_tail = q->b_start = q->b_small;
    q->b_end = q->b_small + ERTS_SMALL_IO_QUEUE;
}

void erts_ioq_clear(ErtsIOQueue *q)
{
    ErtsIOQBinary** binp = q->b_head;
    int driver = q->driver;

    if (q->v_start != q->v_small)
	erts_free(q->alct, (void *) q->v_start);

    while(binp < q->b_tail) {
	if (*binp != NULL)
            free_binary(*binp, driver);
	binp++;
    }
    if (q->b_start != q->b_small)
	erts_free(q->alct, (void *) q->b_start);
    q->v_start = q->v_end = q->v_head = q->v_tail = NULL;
    q->b_start = q->b_end = q->b_head = q->b_tail = NULL;
    q->size = 0;
}

static void free_binary(ErtsIOQBinary *b, int driver)
{
    if (driver)
        driver_free_binary(&b->driver);
    else if (erts_refc_dectest(&b->nif.intern.refc, 0) == 0)
        erts_bin_free(&b->nif);
}

static ErtsIOQBinary *alloc_binary(Uint size, char *source, void **iov_base, int driver)
{
    if (driver) {
        ErlDrvBinary *bin = driver_alloc_binary(size);
        if (!bin) return NULL;
        sys_memcpy(bin->orig_bytes, source, size);
        *iov_base = bin->orig_bytes;
        return (ErtsIOQBinary *)bin;
    } else {
        /* This clause can be triggered in enif_ioq_enq_binary is used */
        Binary *bin = erts_bin_nrml_alloc(size);
        if (!bin) return NULL;
        erts_refc_init(&bin->intern.refc, 1);
        sys_memcpy(bin->orig_bytes, source, size);
        *iov_base = bin->orig_bytes;
        return (ErtsIOQBinary *)bin;
    }
}

Uint erts_ioq_size(ErtsIOQueue *q)
{
    return q->size;
}

/* expand queue to hold n elements in tail or head */
static int expandq(ErtsIOQueue* q, int n, int tail)
/* tail: 0 if make room in head, make room in tail otherwise */
{
    int h_sz;  /* room before header */
    int t_sz;  /* room after tail */
    int q_sz;  /* occupied */
    int nvsz;
    SysIOVec* niov;
    ErtsIOQBinary** nbinv;

    h_sz = q->v_head - q->v_start;
    t_sz = q->v_end -  q->v_tail;
    q_sz = q->v_tail - q->v_head;

    if (tail && (n <= t_sz)) /* do we need to expand tail? */
	return 0;
    else if (!tail && (n <= h_sz))  /* do we need to expand head? */
	return 0;
    else if (n > (h_sz + t_sz)) { /* need to allocate */
	/* we may get little extra but it ok */
	nvsz = (q->v_end - q->v_start) + n;

	niov = erts_alloc_fnf(q->alct, nvsz * sizeof(SysIOVec));
	if (!niov)
	    return -1;
	nbinv = erts_alloc_fnf(q->alct, nvsz * sizeof(ErtsIOQBinary**));
	if (!nbinv) {
	    erts_free(q->alct, (void *) niov);
	    return -1;
	}
	if (tail) {
	    sys_memcpy(niov, q->v_head, q_sz*sizeof(SysIOVec));
	    if (q->v_start != q->v_small)
		erts_free(q->alct, (void *) q->v_start);
	    q->v_start = niov;
	    q->v_end = niov + nvsz;
	    q->v_head = q->v_start;
	    q->v_tail = q->v_head + q_sz;

	    sys_memcpy(nbinv, q->b_head, q_sz*sizeof(ErtsIOQBinary*));
	    if (q->b_start != q->b_small)
		erts_free(q->alct, (void *) q->b_start);
	    q->b_start = nbinv;
	    q->b_end = nbinv + nvsz;
	    q->b_head = q->b_start;
	    q->b_tail = q->b_head + q_sz;
	}
	else {
	    sys_memcpy(niov+nvsz-q_sz, q->v_head, q_sz*sizeof(SysIOVec));
	    if (q->v_start != q->v_small)
		erts_free(q->alct, (void *) q->v_start);
	    q->v_start = niov;
	    q->v_end = niov + nvsz;
	    q->v_tail = q->v_end;
	    q->v_head = q->v_tail - q_sz;

	    sys_memcpy(nbinv+nvsz-q_sz, q->b_head, q_sz*sizeof(ErtsIOQBinary*));
	    if (q->b_start != q->b_small)
		erts_free(q->alct, (void *) q->b_start);
	    q->b_start = nbinv;
	    q->b_end = nbinv + nvsz;
	    q->b_tail = q->b_end;
	    q->b_head = q->b_tail - q_sz;
	}
    }
    else if (tail) {  /* move to beginning to make room in tail */
	sys_memmove(q->v_start, q->v_head, q_sz*sizeof(SysIOVec));
	q->v_head = q->v_start;
	q->v_tail = q->v_head + q_sz;
	sys_memmove(q->b_start, q->b_head, q_sz*sizeof(ErtsIOQBinary*));
	q->b_head = q->b_start;
	q->b_tail = q->b_head + q_sz;
    }
    else {   /* move to end to make room */
	sys_memmove(q->v_end-q_sz, q->v_head, q_sz*sizeof(SysIOVec));
	q->v_tail = q->v_end;
	q->v_head = q->v_tail-q_sz;
	sys_memmove(q->b_end-q_sz, q->b_head, q_sz*sizeof(ErtsIOQBinary*));
	q->b_tail = q->b_end;
	q->b_head = q->b_tail-q_sz;
    }

    return 0;
}

static
int skip(ErtsIOVec* vec, Uint skipbytes,
         SysIOVec **iovp, ErtsIOQBinary ***binvp,
         Uint *lenp)
{
    int n;
    Uint len;
    SysIOVec* iov;
    ErtsIOQBinary** binv;

    if (vec->common.size <= skipbytes)
	return -1;

    iov = vec->common.iov;
    binv = vec->common.binv;
    n = vec->common.vsize;
    /* we use do here to strip iov_len=0 from beginning */
    do {
	len = iov->iov_len;
	if (len <= skipbytes) {
	    skipbytes -= len;
	    iov++;
	    binv++;
	    n--;
	}
	else {
	    iov->iov_base = ((char *)(iov->iov_base)) + skipbytes;
	    iov->iov_len -= skipbytes;
	    skipbytes = 0;
	}
    } while(skipbytes > 0);

    *binvp = binv;
    *iovp = iov;
    *lenp = len;

    return n;
}

/* Put elements from vec at q tail */
int erts_ioq_enqv(ErtsIOQueue *q, ErtsIOVec *eiov, Uint skipbytes)
{
    int n;
    Uint len;
    Uint size = eiov->common.size - skipbytes;
    SysIOVec *iov;
    ErtsIOQBinary** binv;
    ErtsIOQBinary*  b;

    if (q == NULL)
	return -1;

    ASSERT(eiov->common.size >= skipbytes);
    if (eiov->common.size <= skipbytes)
	return 0;

    n = skip(eiov, skipbytes, &iov, &binv, &len);

    if (n < 0)
        return n;

    if (q->v_tail + n >= q->v_end)
	if (expandq(q, n, 1))
            return -1;

    /* Queue and reference all binaries (remove zero length items) */
    while(n--) {
	if ((len = iov->iov_len) > 0) {
	    if ((b = *binv) == NULL) { /* special case create binary ! */
		b = alloc_binary(len, iov->iov_base, (void**)&q->v_tail->iov_base,
                                 q->driver);
                if (!b) return -1;
		*q->b_tail++ = b;
		q->v_tail->iov_len = len;
		q->v_tail++;
	    }
	    else {
                if (q->driver)
                    driver_binary_inc_refc(&b->driver);
                else
                    erts_refc_inc(&b->nif.intern.refc, 1);
		*q->b_tail++ = b;
		*q->v_tail++ = *iov;
	    }
	}
	iov++;
	binv++;
    }
    q->size += size;      /* update total size in queue */
    return 0;
}

/* Put elements from vec at q head */
int erts_ioq_pushqv(ErtsIOQueue *q, ErtsIOVec* vec, Uint skipbytes)
{
    int n;
    Uint len;
    Uint size = vec->common.size - skipbytes;
    SysIOVec* iov;
    ErtsIOQBinary** binv;
    ErtsIOQBinary* b;

    if (q == NULL)
	return -1;

    ASSERT(vec->common.size >= skipbytes);
    if (vec->common.size <= skipbytes)
	return 0;

    n = skip(vec, skipbytes, &iov, &binv, &len);

    if (n < 0)
        return n;

    if (q->v_head - n < q->v_start)
	if (expandq(q, n, 0))
            return -1;

    /* Queue and reference all binaries (remove zero length items) */
    iov += (n-1);  /* move to end */
    binv += (n-1); /* move to end */
    while(n--) {
	if ((len = iov->iov_len) > 0) {
	    if ((b = *binv) == NULL) { /* special case create binary ! */
                if (q->driver) {
                    ErlDrvBinary *bin = driver_alloc_binary(len);
                    if (!bin) return -1;
                    sys_memcpy(bin->orig_bytes, iov->iov_base, len);
                    b = (ErtsIOQBinary *)bin;
                    q->v_head->iov_base = bin->orig_bytes;
                }
		*--q->b_head = b;
		q->v_head--;
		q->v_head->iov_len = len;
	    }
	    else {
                if (q->driver)
                    driver_binary_inc_refc(&b->driver);
                else
                    erts_refc_inc(&b->nif.intern.refc, 1);
		*--q->b_head = b;
		*--q->v_head = *iov;
	    }
	}
	iov--;
	binv--;
    }
    q->size += size;      /* update total size in queue */
    return 0;
}


/*
** Remove size bytes from queue head
** Return number of bytes that remain in queue
*/
int erts_ioq_deq(ErtsIOQueue *q, Uint size)
{
    Uint len;

    if ((q == NULL) || (q->size < size))
	return -1;
    q->size -= size;
    while (size > 0) {
	ASSERT(q->v_head != q->v_tail);

	len = q->v_head->iov_len;
	if (len <= size) {
	    size -= len;
            free_binary(*q->b_head, q->driver);
	    *q->b_head++ = NULL;
	    q->v_head++;
	}
	else {
	    q->v_head->iov_base = ((char *)(q->v_head->iov_base)) + size;
	    q->v_head->iov_len -= size;
	    size = 0;
	}
    }

    /* restart pointers (optimised for enq) */
    if (q->v_head == q->v_tail) {
	q->v_head = q->v_tail = q->v_start;
	q->b_head = q->b_tail = q->b_start;
    }
    return 0;
}


Uint erts_ioq_peekqv(ErtsIOQueue *q, ErtsIOVec *ev) {
    ASSERT(ev);

    if (! q) {
	return (Uint) -1;
    } else {
	if ((ev->common.vsize = q->v_tail - q->v_head) == 0) {
	    ev->common.size = 0;
	    ev->common.iov = NULL;
	    ev->common.binv = NULL;
	} else {
	    ev->common.size = q->size;
	    ev->common.iov = q->v_head;
	    ev->common.binv = q->b_head;
	}
	return q->size;
    }
}

SysIOVec* erts_ioq_peekq(ErtsIOQueue *q, int* vlenp)  /* length of io-vector */
{

    if (q == NULL) {
	*vlenp = -1;
	return NULL;
    }
    if ((*vlenp = (q->v_tail - q->v_head)) == 0)
	return NULL;
    return q->v_head;
}

/* Fills a possibly deep list of chars and binaries into vec
** Small characters are first stored in the buffer buf of length ln
** binaries found are copied and linked into msoh
** Return  vector length on success,
**        -1 on overflow
**        -2 on type error
*/

static ERTS_INLINE void
io_list_to_vec_set_vec(SysIOVec **iov, ErtsIOQBinary ***binv,
                       ErtsIOQBinary *bin, byte *ptr, Uint len,
                       int *vlen)
{
    while (len > MAX_SYSIOVEC_IOVLEN) {
        (*iov)->iov_base = ptr;
        (*iov)->iov_len = MAX_SYSIOVEC_IOVLEN;
        ptr += MAX_SYSIOVEC_IOVLEN;
        len -= MAX_SYSIOVEC_IOVLEN;
        (*iov)++;
        (*vlen)++;
        *(*binv)++ = bin;
    }
    (*iov)->iov_base = ptr;
    (*iov)->iov_len = len;
    *(*binv)++ = bin;
    (*iov)++;
    (*vlen)++;
}

int
erts_ioq_iolist_to_vec(Eterm obj,	  /* io-list */
                       SysIOVec* iov,	  /* io vector */
                       ErtsIOQBinary** binv,       /* binary reference vector */
                       ErtsIOQBinary* cbin,        /* binary to store characters */
                       Uint bin_limit,  /* small binaries limit */
                       int driver)
{
    DECLARE_ESTACK(s);
    Eterm* objp;
    byte *buf  = NULL;
    Uint len = 0;
    Uint csize  = 0;
    int vlen   = 0;
    byte* cptr;

    if (cbin) {
        if (driver) {
            buf = (byte*)cbin->driver.orig_bytes;
            len = cbin->driver.orig_size;
        } else {
            buf = (byte*)cbin->nif.orig_bytes;
            len = cbin->nif.orig_size;
        }
    }
    cptr = buf;

    goto L_jump_start;  /* avoid push */

    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_jump_start:
	if (is_list(obj)) {
	L_iter_list:
	    objp = list_val(obj);
	    obj = CAR(objp);
	    if (is_byte(obj)) {
		if (len == 0)
		    goto L_overflow;
		*buf++ = unsigned_val(obj);
		csize++;
		len--;
	    } else if (is_bitstring(obj)) {
		ESTACK_PUSH(s, CDR(objp));
		goto handle_binary;
	    } else if (is_list(obj)) {
		ESTACK_PUSH(s, CDR(objp));
		goto L_iter_list;    /* on head */
	    } else if (!is_nil(obj)) {
		goto L_type_error;
	    }
	    obj = CDR(objp);
	    if (is_list(obj))
		goto L_iter_list; /* on tail */
	    else if (is_bitstring(obj)) {
		goto handle_binary;
	    } else if (!is_nil(obj)) {
		goto L_type_error;
	    }
	} else if (is_bitstring(obj)) {
            ERTS_DECLARE_DUMMY(Eterm br_flags);
            Uint size_in_bytes;
            Uint offset, size;
            byte *base;
            BinRef *br;

        handle_binary:
            ERTS_PIN_BITSTRING(obj, br_flags, br, base, offset, size);
            ASSERT(TAIL_BITS(size) == 0);

            size_in_bytes = BYTE_SIZE(size);

            if ((br == NULL) ||
                BIT_OFFSET(offset) != 0 ||
                (size_in_bytes < bin_limit)) {
                if (len < size_in_bytes) {
                    goto L_overflow;
                }

                copy_binary_to_buffer(buf, 0, base, offset, size);

                csize += size_in_bytes;
                buf += size_in_bytes;
                len -= size_in_bytes;
            } else {
                ErtsIOQBinary *qbin;

                if (csize != 0) {
                    io_list_to_vec_set_vec(&iov, &binv, cbin,
                                           cptr, csize, &vlen);
                    cptr = buf;
                    csize = 0;
                }

                if (driver) {
                    qbin = (ErtsIOQBinary*)Binary2ErlDrvBinary(br->val);
                } else {
                    qbin = (ErtsIOQBinary*)br->val;
                }

                ASSERT(BIT_OFFSET(offset) == 0);
                io_list_to_vec_set_vec(&iov, &binv, qbin,
                                       &base[BYTE_OFFSET(offset)],
                                       size_in_bytes, &vlen);
            }
        } else if (!is_nil(obj)) {
            goto L_type_error;
        }
    }

    if (csize != 0) {
        io_list_to_vec_set_vec(&iov, &binv, cbin, cptr, csize, &vlen);
    }

    DESTROY_ESTACK(s);
    return vlen;

 L_type_error:
    DESTROY_ESTACK(s);
    return -2;

 L_overflow:
    DESTROY_ESTACK(s);
    return -1;
}

static ERTS_INLINE int
io_list_vec_count(Eterm obj, Uint *v_size,
                  Uint *c_size, Uint *b_size, Uint *in_clist,
                  Uint *p_v_size, Uint *p_c_size, Uint *p_in_clist,
                  Uint blimit)
{
    ERTS_DECLARE_DUMMY(Eterm br_flags);
    ERTS_DECLARE_DUMMY(byte *base);
    Uint offset, size;
    BinRef *br;

    ERTS_GET_BITSTRING_REF(obj, br_flags, br, base, offset, size);

    if (TAIL_BITS(size) != 0) {
        return 1;
    }
    size = BYTE_SIZE(size);

    if (br && BIT_OFFSET(offset) == 0) {
	*b_size += size;
        if (*b_size < size) return 2;
	*in_clist = 0;
        ++*v_size;
        /* If iov_len is smaller then Uint we split the binary into*/
        /* multiple smaller (2GB) elements in the iolist.*/
	*v_size += size / MAX_SYSIOVEC_IOVLEN;
        if (size >= blimit) {
            *p_in_clist = 0;
            ++*p_v_size;
        } else {
            *p_c_size += size;
            if (!*p_in_clist) {
                *p_in_clist = 1;
                ++*p_v_size;
            }
        }
    } else {
	*c_size += size;
        if (*c_size < size) return 2;
	if (!*in_clist) {
	    *in_clist = 1;
	    ++*v_size;
	}
	*p_c_size += size;
	if (!*p_in_clist) {
	    *p_in_clist = 1;
	    ++*p_v_size;
	}
    }

    return 0;
}

#define IO_LIST_VEC_COUNT(obj)                                          \
    do {                                                                \
        switch (io_list_vec_count(obj, &v_size, &c_size,                \
                                  &b_size, &in_clist,                   \
                                  &p_v_size, &p_c_size, &p_in_clist,    \
                                  blimit)) {                            \
        case 1: goto L_type_error;                                      \
        case 2: goto L_overflow_error;                                  \
        default: break;                                                 \
        }                                                               \
    } while(0)

/* 
 * Returns 0 if successful and a non-zero value otherwise.
 *
 * Return values through pointers:
 *    *vsize      - SysIOVec size needed for a writev
 *    *csize      - Number of bytes not in binary (in the common binary)
 *    *pvsize     - SysIOVec size needed if packing small binaries
 *    *pcsize     - Number of bytes in the common binary if packing
 *    *total_size - Total size of iolist in bytes
 */
int
erts_ioq_iolist_vec_len(Eterm obj, int* vsize, Uint* csize,
                        Uint* pvsize, Uint* pcsize,
                        size_t* total_size, Uint blimit)
{
    DECLARE_ESTACK(s);
    Eterm* objp;
    Uint v_size = 0;
    Uint c_size = 0;
    Uint b_size = 0;
    Uint in_clist = 0;
    Uint p_v_size = 0;
    Uint p_c_size = 0;
    Uint p_in_clist = 0;
    size_t total;

    goto L_jump_start;  /* avoid a push */

    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_jump_start:
	if (is_list(obj)) {
	L_iter_list:
	    objp = list_val(obj);
	    obj = CAR(objp);

	    if (is_byte(obj)) {
		c_size++;
		if (c_size == 0) {
		    goto L_overflow_error;
		}
		if (!in_clist) {
		    in_clist = 1;
		    v_size++;
		}
		p_c_size++;
		if (!p_in_clist) {
		    p_in_clist = 1;
		    p_v_size++;
		}
	    }
	    else if (is_bitstring(obj)) {
                IO_LIST_VEC_COUNT(obj);
	    }
	    else if (is_list(obj)) {
		ESTACK_PUSH(s, CDR(objp));
		goto L_iter_list;   /* on head */
	    }
	    else if (!is_nil(obj)) {
		goto L_type_error;
	    }

	    obj = CDR(objp);
	    if (is_list(obj))
		goto L_iter_list;   /* on tail */
	    else if (is_bitstring(obj)) {  /* binary tail is OK */
		IO_LIST_VEC_COUNT(obj);
	    }
	    else if (!is_nil(obj)) {
		goto L_type_error;
	    }
	}
	else if (is_bitstring(obj)) {
	    IO_LIST_VEC_COUNT(obj);
	}
	else if (!is_nil(obj)) {
	    goto L_type_error;
	}
    }

    total = c_size + b_size;
    if (total < c_size) {
	goto L_overflow_error;
    }
    *total_size = total;

    DESTROY_ESTACK(s);
    *vsize = v_size;
    *csize = c_size;
    *pvsize = p_v_size;
    *pcsize = p_c_size;
    return 0;

 L_type_error:
 L_overflow_error:
    DESTROY_ESTACK(s);
    return 1;
}

typedef struct {
    Eterm *result_tail;
    Eterm result_head;
    Eterm input_list;

    struct erl_off_heap_header **off_heap_tail;
    ErlOffHeap off_heap;
    UWord acc_offset;
    UWord acc_size;
    BinRef *acc_ref;
    Binary *acc;

    /* We yield after copying this many bytes into the accumulator (minus
     * eating a few on consing etc). */
    UWord reds_available;
    UWord reds_spent;

    Process *process;
    ErtsEStack estack;

    Eterm magic_reference;
} iol2v_state_t;

static void iol2v_clean_offheap(iol2v_state_t *state) {
    /* Free all the off-heap binaries we've allocated but not linked into the
     * process' off-heap list. */
    struct erl_off_heap_header *bin_ref = state->off_heap.first;

    while (bin_ref != NULL) {
        Binary *refc_binary;

        ASSERT(bin_ref->thing_word == HEADER_BIN_REF);

        refc_binary = ((BinRef*)bin_ref)->val;
        ASSERT(erts_refc_read(&refc_binary->intern.refc, 1) == 1);
        erts_bin_free(refc_binary);

        ASSERT(bin_ref->next != NULL || state->off_heap_tail == &bin_ref->next);
        bin_ref = bin_ref->next;
    }

    state->off_heap.first = NULL;
}

static int iol2v_state_destructor(Binary *data) {
    iol2v_state_t *state = ERTS_MAGIC_BIN_UNALIGNED_DATA(data);

    DESTROY_SAVED_ESTACK(&state->estack);
    iol2v_clean_offheap(state);

    return 1;
}

static void iol2v_init(iol2v_state_t *state, Process *process, Eterm input) {
    state->process = process;

    state->result_tail = &state->result_head;
    state->result_head = NIL;
    state->input_list = input;

    state->magic_reference = NIL;
    state->acc_ref = NULL;
    state->acc_size = 0;
    state->acc = NULL;

    state->off_heap_tail = &state->off_heap.first;
    state->off_heap.first = NULL;
    state->off_heap.overhead = 0;

    CLEAR_SAVED_ESTACK(&state->estack);
}

static void iol2v_finish_accumulator(iol2v_state_t *state) {
    Binary *accumulator;
    BinRef *acc_ref;
    Uint size;

    accumulator = state->acc;
    state->acc = NULL;

    acc_ref = state->acc_ref;
    ASSERT(acc_ref->val == accumulator);

    /* Our allocators are 8-byte aligned, so don't bother reallocating for
     * differences smaller than that. */
    size = state->acc_offset + state->acc_size;
    if (size < (accumulator->orig_size + 8)) {
        acc_ref->val = erts_bin_realloc(accumulator, size);
    }
}

/* Destructively enqueues a term to the result list, saving us the hassle of
 * having to reverse it later. This is safe since GC is disabled and we never
 * leak the unfinished term to the outside. */
static void iol2v_enqueue_result(iol2v_state_t *state, Eterm term) {
    Eterm *hp = HAlloc(state->process, 2);

    *state->result_tail = CONS(hp, term, NIL);
    state->result_tail = &hp[1];

    state->reds_spent += 1;
}

static Eterm iol2v_extract_acc_term(iol2v_state_t *state) {
    ErlSubBits *sb;
    Eterm *hp;

    hp = HAlloc(state->process, ERL_SUB_BITS_SIZE);

    /* We mark all our produced binaries as volatile as they may be reallocated
     * when we shrink the final accumulator. As the underlying binary isn't
     * writable, they will be lazily made non-volatile the first time they are
     * accessed.
     *
     * (The base pointer is left NULL to catch errors in the aforementioned
     * code) */
    sb = (ErlSubBits*)hp;
    erl_sub_bits_init(sb,
                      ERL_SUB_BITS_FLAG_VOLATILE,
                      make_boxed((Eterm*)state->acc_ref),
                      NULL,
                      NBITS(state->acc_offset),
                      NBITS(state->acc_size));

    state->acc_offset += state->acc_size;
    state->acc_size = 0;

    return make_bitstring(sb);
}

static Uint iol2v_expand_acc(iol2v_state_t *state, UWord extra) {
    Binary *refc_binary;
    BinRef *br;

    ASSERT(extra >= 1);

    if (state->acc != NULL) {
        UWord required_bytes, available_bytes;

        available_bytes = (state->acc)->orig_size;
        available_bytes -= state->acc_offset + state->acc_size;
        required_bytes = state->acc_size + extra;

        if (required_bytes <= available_bytes) {
            return extra;
        }

        iol2v_enqueue_result(state, iol2v_extract_acc_term(state));
        iol2v_finish_accumulator(state);
    }

    refc_binary = erts_bin_nrml_alloc(MAX(extra, IOL2V_ACC_SIZE));

    br = (BinRef*)HAlloc(state->process, ERL_BIN_REF_SIZE);
    br->thing_word = HEADER_BIN_REF;
    br->val = refc_binary;

    (*state->off_heap_tail) = (struct erl_off_heap_header*)br;
    state->off_heap_tail = &br->next;
    ERTS_BR_OVERHEAD(&state->off_heap, br);
    br->next = NULL;

    state->acc = refc_binary;
    state->acc_ref = br;
    state->acc_offset = 0;
    state->acc_size = 0;

    return extra;
}

static int iol2v_append_byte_seq(iol2v_state_t *state,
                                 Eterm seq_start,
                                 Eterm *seq_end) {
    Eterm lookahead, iterator;
    Uint observed_bits;
    SWord seq_length;
    byte *acc_data, *acc_end;

    lookahead = seq_start;
    seq_length = 0;

    ASSERT(state->reds_available > state->reds_spent);

    while (is_list(lookahead)) {
        Eterm *cell = list_val(lookahead);

        if (!is_small(CAR(cell))) {
            break;
        }

        if (seq_length * 2 >= (state->reds_available - state->reds_spent)) {
            break;
        }

        lookahead = CDR(cell);
        seq_length += 1;
    }

    ASSERT(seq_length >= 1);

    /* Bump a few extra reductions to account for list traversal. */
    state->reds_spent += seq_length;
    iterator = seq_start;
    observed_bits = 0;

    while (seq_length > 0) {
        Uint to_copy = iol2v_expand_acc(state, seq_length);

        acc_data = (byte*)&(state->acc)->orig_bytes;
        acc_data += state->acc_offset + state->acc_size;
        acc_end = acc_data + to_copy;

        state->acc_size += to_copy;
        seq_length -= to_copy;

        ASSERT(acc_data < acc_end);

        do {
            Eterm *cell;
            Uint byte;

            cell = list_val(iterator);
            iterator = CDR(cell);

            byte = unsigned_val(CAR(cell));
            observed_bits |= byte;

            *(acc_data++) = byte;
        } while (acc_data < acc_end);

        if (observed_bits > UCHAR_MAX) {
            return 0;
        }
    }

    ASSERT(iterator == lookahead);
    *seq_end = iterator;

    return 1;
}

static int iol2v_append_binary(iol2v_state_t *state, Eterm bin_term) {
    ERTS_DECLARE_DUMMY(Eterm br_flags);
    Uint offset, size;
    BinRef *br;
    byte *base;

    ASSERT(state->reds_available > state->reds_spent);

    ERTS_PIN_BITSTRING(bin_term, br_flags, br, base, offset, size);

    if (TAIL_BITS(size) != 0) {
        return 0;
    } else if (size == 0) {
        state->reds_spent += 1;
        return 1;
    }

    size = BYTE_SIZE(size);

    if (br && BIT_OFFSET(offset) == 0) {
        if (state->acc_size != 0) {
            iol2v_enqueue_result(state, iol2v_extract_acc_term(state));
        }

        iol2v_enqueue_result(state, bin_term);
    } else {
        state->reds_spent += size;

        while (size > 0) {
            Uint to_copy;
            byte *dst;

            to_copy = iol2v_expand_acc(state, size);
            dst = (byte*)&(state->acc)->orig_bytes[state->acc_offset],

            copy_binary_to_buffer(dst,
                                  NBITS(state->acc_size),
                                  base, offset,
                                  NBITS(to_copy));

            offset += NBITS(to_copy);
            size -= to_copy;

            state->acc_size += to_copy;
        }
    }

    return 1;
}

static BIF_RETTYPE iol2v_yield(iol2v_state_t *state) {
    if (is_nil(state->magic_reference)) {
        iol2v_state_t *boxed_state;
        Binary *magic_binary;
        Eterm *hp;

        magic_binary = erts_create_magic_binary_x(sizeof(*state),
            &iol2v_state_destructor, ERTS_ALC_T_BINARY, 1);

        boxed_state = ERTS_MAGIC_BIN_UNALIGNED_DATA(magic_binary);
        sys_memcpy(boxed_state, state, sizeof(*state));

        hp = HAlloc(boxed_state->process, ERTS_MAGIC_REF_THING_SIZE);
        boxed_state->magic_reference =
            erts_mk_magic_ref(&hp, &MSO(boxed_state->process), magic_binary);

        if (state->result_tail == &state->result_head) {
            boxed_state->result_tail = &boxed_state->result_head;
        }

        if (state->off_heap_tail == &state->off_heap.first) {
            boxed_state->off_heap_tail = &boxed_state->off_heap.first;
        }

        state = boxed_state;
    }

    ERTS_BIF_YIELD1(BIF_TRAP_EXPORT(BIF_iolist_to_iovec_1),
        state->process, state->magic_reference);
}

static BIF_RETTYPE iol2v_continue(iol2v_state_t *state) {
    Eterm iterator;

    DECLARE_ESTACK(s);
    ESTACK_CHANGE_ALLOCATOR(s, ERTS_ALC_T_SAVED_ESTACK);

    state->reds_available =
        ERTS_BIF_REDS_LEFT(state->process) * IOL2V_BYTES_PER_REDUCTION;
    state->reds_spent = 0;

    if (state->estack.start) {
        ESTACK_RESTORE(s, &state->estack);
    }

    iterator = state->input_list;

    for(;;) {
        if (state->reds_spent >= state->reds_available) {
            ESTACK_SAVE(s, &state->estack);
            state->input_list = iterator;

            return iol2v_yield(state);
        }

        while (is_list(iterator)) {
            Eterm *cell;
            Eterm head;

            cell = list_val(iterator);
            head = CAR(cell);

            if (is_bitstring(head)) {
                if (!iol2v_append_binary(state, head)) {
                    goto l_badarg;
                }

                iterator = CDR(cell);
            } else if (is_small(head)) {
                Eterm seq_end;

                if (!iol2v_append_byte_seq(state, iterator, &seq_end)) {
                    goto l_badarg;
                }

                iterator = seq_end;
            } else if (is_list(head) || is_nil(head)) {
                Eterm tail = CDR(cell);

                if (!is_nil(tail)) {
                    ESTACK_PUSH(s, tail);
                }

                state->reds_spent += 1;
                iterator = head;
            } else {
                goto l_badarg;
            }

            if (state->reds_spent >= state->reds_available) {
                ESTACK_SAVE(s, &state->estack);
                state->input_list = iterator;

                return iol2v_yield(state);
            }
        }

        if (is_bitstring(iterator)) {
            if (!iol2v_append_binary(state, iterator)) {
                goto l_badarg;
            }
        } else if (!is_nil(iterator)) {
            goto l_badarg;
        }

        if(ESTACK_ISEMPTY(s)) {
            break;
        }

        iterator = ESTACK_POP(s);
    }

    if (state->acc_size != 0) {
        ASSERT(state->acc);
        iol2v_enqueue_result(state, iol2v_extract_acc_term(state));
    }

    if (state->acc) {
        iol2v_finish_accumulator(state);

        /* Link the state's off-heap list into the process. */
        ASSERT(state->off_heap.first != NULL && state->acc_ref != NULL);
        *(state->off_heap_tail) = MSO(state->process).first;

        MSO(state->process).first = state->off_heap.first;
        MSO(state->process).overhead += state->off_heap.overhead;

        state->off_heap.first = NULL;
    }

    ASSERT(state->off_heap.first == NULL);

    BUMP_REDS(state->process, state->reds_spent / IOL2V_BYTES_PER_REDUCTION);

    CLEAR_SAVED_ESTACK(&state->estack);
    DESTROY_ESTACK(s);

    BIF_RET(state->result_head);

l_badarg:
    iol2v_clean_offheap(state);

    CLEAR_SAVED_ESTACK(&state->estack);
    DESTROY_ESTACK(s);

    BIF_ERROR(state->process, BADARG);
}

BIF_RETTYPE iolist_to_iovec_1(BIF_ALIST_1) {
    BIF_RETTYPE result;

    if (is_nil(BIF_ARG_1)) {
        BIF_RET(NIL);
    } else if (is_bitstring(BIF_ARG_1)) {
        Uint size = bitstring_size(BIF_ARG_1);

        if (TAIL_BITS(size) != 0) {
            ASSERT(!(BIF_P->flags & F_DISABLE_GC));
            BIF_ERROR(BIF_P, BADARG);
        } else if (size > 0) {
            Eterm *hp = HAlloc(BIF_P, 2);

            BIF_RET(CONS(hp, BIF_ARG_1, NIL));
        } else {
            BIF_RET(NIL);
        }
    } else if (is_internal_magic_ref(BIF_ARG_1)) {
        iol2v_state_t *state;
        Binary *magic;

        magic = erts_magic_ref2bin(BIF_ARG_1);

        if (ERTS_MAGIC_BIN_DESTRUCTOR(magic) != &iol2v_state_destructor) {
            ASSERT(!(BIF_P->flags & F_DISABLE_GC));
            BIF_ERROR(BIF_P, BADARG);
        }

        ASSERT(BIF_P->flags & F_DISABLE_GC);

        state = ERTS_MAGIC_BIN_UNALIGNED_DATA(magic);
        result = iol2v_continue(state);
    } else if (!is_list(BIF_ARG_1)) {
        ASSERT(!(BIF_P->flags & F_DISABLE_GC));
        BIF_ERROR(BIF_P, BADARG);
    } else {
        iol2v_state_t state;

        iol2v_init(&state, BIF_P, BIF_ARG_1);

        erts_set_gc_state(BIF_P, 0);

        result = iol2v_continue(&state);
    }

    if (result != THE_NON_VALUE || BIF_P->freason != TRAP) {
        erts_set_gc_state(BIF_P, 1);
    }

    BIF_RET(result);
}
