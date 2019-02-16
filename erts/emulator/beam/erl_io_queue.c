/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2017-2018. All Rights Reserved.
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

#define ERL_WANT_HIPE_BIF_WRAPPER__
#include "bif.h"
#undef ERL_WANT_HIPE_BIF_WRAPPER__

#include "erl_bits.h"
#include "erl_io_queue.h"

#define IOL2V_SMALL_BIN_LIMIT (ERL_ONHEAP_BIN_LIMIT * 4)

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
** Return  vector length on succsess,
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
	    } else if (is_binary(obj)) {
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
	    else if (is_binary(obj)) {
		goto handle_binary;
	    } else if (!is_nil(obj)) {
		goto L_type_error;
	    }
	} else if (is_binary(obj)) {
	    Eterm real_bin;
	    Uint offset;
	    Eterm* bptr;
	    Uint size;
	    int bitoffs;
	    int bitsize;

	handle_binary:
	    size = binary_size(obj);
	    ERTS_GET_REAL_BIN(obj, real_bin, offset, bitoffs, bitsize);
	    ASSERT(bitsize == 0);
	    bptr = binary_val(real_bin);
	    if (*bptr == HEADER_PROC_BIN) {
		ProcBin* pb = (ProcBin *) bptr;
		if (bitoffs != 0) {
		    if (len < size) {
			goto L_overflow;
		    }
		    erts_copy_bits(pb->bytes+offset, bitoffs, 1,
				   (byte *) buf, 0, 1, size*8);
		    csize += size;
		    buf += size;
		    len -= size;
		} else if (bin_limit && size < bin_limit) {
		    if (len < size) {
			goto L_overflow;
		    }
		    sys_memcpy(buf, pb->bytes+offset, size);
		    csize += size;
		    buf += size;
		    len -= size;
		} else {
                    ErtsIOQBinary *qbin;
		    if (csize != 0) {
                        io_list_to_vec_set_vec(&iov, &binv, cbin,
                                               cptr, csize, &vlen);
			cptr = buf;
			csize = 0;
		    }
		    if (pb->flags) {
			erts_emasculate_writable_binary(pb);
		    }
                    if (driver)
                        qbin = (ErtsIOQBinary*)Binary2ErlDrvBinary(pb->val);
                    else
                        qbin = (ErtsIOQBinary*)pb->val;

                    io_list_to_vec_set_vec(
                        &iov, &binv, qbin,
                        pb->bytes+offset, size, &vlen);
		}
	    } else {
		ErlHeapBin* hb = (ErlHeapBin *) bptr;
		if (len < size) {
		    goto L_overflow;
		}
		copy_binary_to_buffer(buf, 0,
				      ((byte *) hb->data)+offset, bitoffs,
				      8*size);
		csize += size;
		buf += size;
		len -= size;
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
    Uint size = binary_size(obj);
    Eterm real;
    ERTS_DECLARE_DUMMY(Uint offset);
    int bitoffs;
    int bitsize;
    ERTS_GET_REAL_BIN(obj, real, offset, bitoffs, bitsize);
    if (bitsize != 0) return 1;
    if (thing_subtag(*binary_val(real)) == REFC_BINARY_SUBTAG &&
	bitoffs == 0) {
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
	    else if (is_binary(obj)) {
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
	    else if (is_binary(obj)) {  /* binary tail is OK */
		IO_LIST_VEC_COUNT(obj);
	    }
	    else if (!is_nil(obj)) {
		goto L_type_error;
	    }
	}
	else if (is_binary(obj)) {
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
    Eterm result_head;
    Eterm result_tail;
    Eterm input_list;

    UWord acc_size;
    Binary *acc;

    /* We yield after copying this many bytes into the accumulator (Minus
     * eating a few on consing etc). Large binaries will only count to the
     * extent their split (if any) resulted in a copy op. */
    UWord bytereds_available;
    UWord bytereds_spent;

    Process *process;
    ErtsEStack estack;

    Eterm magic_reference;
} iol2v_state_t;

static int iol2v_state_destructor(Binary *data) {
    iol2v_state_t *state = ERTS_MAGIC_BIN_UNALIGNED_DATA(data);

    DESTROY_SAVED_ESTACK(&state->estack);

    if (state->acc != NULL) {
        erts_bin_free(state->acc);
    }

    return 1;
}

static void iol2v_init(iol2v_state_t *state, Process *process, Eterm input) {
    state->process = process;

    state->result_head = NIL;
    state->result_tail = NIL;
    state->input_list = input;

    state->magic_reference = NIL;
    state->acc_size = 0;
    state->acc = NULL;

    CLEAR_SAVED_ESTACK(&state->estack);
}

static Eterm iol2v_make_sub_bin(iol2v_state_t *state, Eterm bin_term,
        UWord offset, UWord size) {
    Uint byte_offset, bit_offset, bit_size;
    ErlSubBin *sb;
    Eterm orig_pb_term;

    sb = (ErlSubBin*)HAlloc(state->process, ERL_SUB_BIN_SIZE);

    ERTS_GET_REAL_BIN(bin_term, orig_pb_term,
        byte_offset, bit_offset, bit_size);

    ASSERT(bit_size == 0);

    sb->thing_word = HEADER_SUB_BIN;
    sb->bitoffs = bit_offset;
    sb->bitsize = 0;
    sb->orig = orig_pb_term;
    sb->is_writable = 0;

    sb->offs = byte_offset + offset;
    sb->size = size;

    return make_binary(sb);
}

static Eterm iol2v_promote_acc(iol2v_state_t *state) {
    Eterm bin;

    bin = erts_build_proc_bin(&MSO(state->process),
                              HAlloc(state->process, PROC_BIN_SIZE),
                              erts_bin_realloc(state->acc, state->acc_size));
    state->acc_size = 0;
    state->acc = NULL;

    return bin;
}

/* Destructively enqueues a term to the result list, saving us the hassle of
 * having to reverse it later. This is safe since GC is disabled and we never
 * leak the unfinished term to the outside. */
static void iol2v_enqueue_result(iol2v_state_t *state, Eterm term) {
    Eterm prev_tail;
    Eterm *hp;

    prev_tail = state->result_tail;

    hp = HAlloc(state->process, 2);
    state->result_tail = CONS(hp, term, NIL);

    if(prev_tail != NIL) {
        Eterm *prev_cell = list_val(prev_tail);
        CDR(prev_cell) = state->result_tail;
    } else {
        state->result_head = state->result_tail;
    }

    state->bytereds_spent += 1;
}

#ifndef DEBUG
    #define ACC_REALLOCATION_LIMIT (IOL2V_SMALL_BIN_LIMIT * 32)
#else
    #define ACC_REALLOCATION_LIMIT (IOL2V_SMALL_BIN_LIMIT * 4)
#endif

static void iol2v_expand_acc(iol2v_state_t *state, UWord extra) {
    UWord required_bytes, acc_alloc_size;

    ERTS_CT_ASSERT(ERTS_UWORD_MAX > ACC_REALLOCATION_LIMIT / 2);
    ASSERT(extra >= 1);

    acc_alloc_size = state->acc != NULL ? (state->acc)->orig_size : 0;
    required_bytes = state->acc_size + extra;

    if (state->acc == NULL) {
        UWord new_size = MAX(required_bytes, IOL2V_SMALL_BIN_LIMIT);

        state->acc = erts_bin_nrml_alloc(new_size);
    } else if (required_bytes > acc_alloc_size) {
        Binary *prev_acc;
        UWord new_size;

        if (acc_alloc_size >= ACC_REALLOCATION_LIMIT) {
            /* We skip reallocating once we hit a certain point; it often
             * results in extra copying and we're very likely to overallocate
             * on anything other than absurdly long byte/heapbin sequences. */
            iol2v_enqueue_result(state, iol2v_promote_acc(state));
            iol2v_expand_acc(state, extra);
            return;
        }

        new_size = MAX(required_bytes, acc_alloc_size * 2);
        prev_acc = state->acc;

        state->acc = erts_bin_realloc(prev_acc, new_size);

        if (prev_acc != state->acc) {
            state->bytereds_spent += state->acc_size;
        }
    }

    state->bytereds_spent += extra;
}

static int iol2v_append_byte_seq(iol2v_state_t *state, Eterm seq_start, Eterm *seq_end) {
    Eterm lookahead, iterator;
    Uint observed_bits;
    SWord seq_length;
    char *acc_data;

    lookahead = seq_start;
    seq_length = 0;

    ASSERT(state->bytereds_available > state->bytereds_spent);

    while (is_list(lookahead)) {
        Eterm *cell = list_val(lookahead);

        if (!is_small(CAR(cell))) {
            break;
        }

        if (seq_length * 2 >= (state->bytereds_available - state->bytereds_spent)) {
            break;
        }

        lookahead = CDR(cell);
        seq_length += 1;
    }

    ASSERT(seq_length >= 1);

    iol2v_expand_acc(state, seq_length);

    /* Bump a few extra reductions to account for list traversal. */
    state->bytereds_spent += seq_length;

    acc_data = &(state->acc)->orig_bytes[state->acc_size];
    state->acc_size += seq_length;

    iterator = seq_start;
    observed_bits = 0;

    while (iterator != lookahead) {
        Eterm *cell;
        Uint byte;

        cell = list_val(iterator);
        iterator = CDR(cell);

        byte = unsigned_val(CAR(cell));
        observed_bits |= byte;

        ASSERT(acc_data < &(state->acc)->orig_bytes[state->acc_size]);
        *(acc_data++) = byte;
    }

    if (observed_bits > UCHAR_MAX) {
        return 0;
    }

    ASSERT(acc_data == &(state->acc)->orig_bytes[state->acc_size]);
    *seq_end = iterator;

    return 1;
}

static int iol2v_append_binary(iol2v_state_t *state, Eterm bin_term) {
    int is_acc_small, is_bin_small;
    UWord combined_size;
    UWord binary_size;

    Uint byte_offset, bit_offset, bit_size;
    byte *binary_data;

    Eterm *parent_header;
    Eterm parent_binary;

    ASSERT(state->bytereds_available > state->bytereds_spent);

    ERTS_GET_REAL_BIN(bin_term, parent_binary, byte_offset, bit_offset, bit_size);
    parent_header = binary_val(parent_binary);
    binary_size = binary_size(bin_term);

    if (bit_size != 0) {
        return 0;
    } else if (binary_size == 0) {
        state->bytereds_spent += 1;
        return 1;
    }

    is_acc_small = state->acc_size < IOL2V_SMALL_BIN_LIMIT;
    is_bin_small = binary_size < IOL2V_SMALL_BIN_LIMIT;
    combined_size = binary_size + state->acc_size;

    if (thing_subtag(*parent_header) == REFC_BINARY_SUBTAG) {
        ProcBin *pb = (ProcBin*)parent_header;

        if (pb->flags) {
            erts_emasculate_writable_binary(pb);
        }

        binary_data = &((byte*)pb->bytes)[byte_offset];
    } else {
        ErlHeapBin *hb = (ErlHeapBin*)parent_header;

        ASSERT(thing_subtag(*parent_header) == HEAP_BINARY_SUBTAG);
        ASSERT(is_bin_small);

        binary_data = &((byte*)&hb->data)[byte_offset];
    }

    if (!is_bin_small && (state->acc_size == 0 || !is_acc_small)) {
        /* Avoid combining if we encounter an acceptably large binary while the
         * accumulator is either empty or large enough to be returned on its
         * own. */
        if (state->acc_size != 0) {
            iol2v_enqueue_result(state, iol2v_promote_acc(state));
        }

        iol2v_enqueue_result(state, bin_term);
    } else if (is_bin_small || combined_size < (IOL2V_SMALL_BIN_LIMIT * 2)) {
        /* If the candidate is small or we can't split the combination in two,
         * then just copy it into the accumulator. */
        iol2v_expand_acc(state, binary_size);

        if (ERTS_LIKELY(bit_offset == 0)) {
            sys_memcpy(&(state->acc)->orig_bytes[state->acc_size],
                binary_data, binary_size);
        } else {
            ASSERT(binary_size <= ERTS_UWORD_MAX / 8);

            erts_copy_bits(binary_data, bit_offset, 1,
                (byte*)&(state->acc)->orig_bytes[state->acc_size], 0, 1,
                binary_size * 8);
        }

        state->acc_size += binary_size;
    } else {
        /* Otherwise, append enough data for the accumulator to be valid, and
         * then return the rest as a sub-binary. */
        UWord spill = IOL2V_SMALL_BIN_LIMIT - state->acc_size;
        Eterm binary_tail;

        iol2v_expand_acc(state, spill);

        if (ERTS_LIKELY(bit_offset == 0)) {
            sys_memcpy(&(state->acc)->orig_bytes[state->acc_size],
                binary_data, spill);
        } else {
            ASSERT(binary_size <= ERTS_UWORD_MAX / 8);

            erts_copy_bits(binary_data, bit_offset, 1,
                (byte*)&(state->acc)->orig_bytes[state->acc_size], 0, 1,
                spill * 8);
        }

        state->acc_size += spill;

        binary_tail = iol2v_make_sub_bin(state, bin_term, spill,
            binary_size - spill);

        iol2v_enqueue_result(state, iol2v_promote_acc(state));
        iol2v_enqueue_result(state, binary_tail);
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

        state = boxed_state;
    }

    ERTS_BIF_YIELD1(bif_export[BIF_iolist_to_iovec_1],
        state->process, state->magic_reference);
}

static BIF_RETTYPE iol2v_continue(iol2v_state_t *state) {
    Eterm iterator;

    DECLARE_ESTACK(s);
    ESTACK_CHANGE_ALLOCATOR(s, ERTS_ALC_T_SAVED_ESTACK);

    state->bytereds_available =
        ERTS_BIF_REDS_LEFT(state->process) * IOL2V_SMALL_BIN_LIMIT;
    state->bytereds_spent = 0;

    if (state->estack.start) {
        ESTACK_RESTORE(s, &state->estack);
    }

    iterator = state->input_list;

    for(;;) {
        if (state->bytereds_spent >= state->bytereds_available) {
            ESTACK_SAVE(s, &state->estack);
            state->input_list = iterator;

            return iol2v_yield(state);
        }

        while (is_list(iterator)) {
            Eterm *cell;
            Eterm head;

            cell = list_val(iterator);
            head = CAR(cell);

            if (is_binary(head)) {
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

                state->bytereds_spent += 1;
                iterator = head;
            } else {
                goto l_badarg;
            }

            if (state->bytereds_spent >= state->bytereds_available) {
                ESTACK_SAVE(s, &state->estack);
                state->input_list = iterator;

                return iol2v_yield(state);
            }
        }

        if (is_binary(iterator)) {
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
        iol2v_enqueue_result(state, iol2v_promote_acc(state));
    }

    BUMP_REDS(state->process, state->bytereds_spent / IOL2V_SMALL_BIN_LIMIT);

    CLEAR_SAVED_ESTACK(&state->estack);
    DESTROY_ESTACK(s);

    BIF_RET(state->result_head);

l_badarg:
    CLEAR_SAVED_ESTACK(&state->estack);
    DESTROY_ESTACK(s);

    if (state->acc != NULL) {
        erts_bin_free(state->acc);
        state->acc = NULL;
    }

    BIF_ERROR(state->process, BADARG);
}

HIPE_WRAPPER_BIF_DISABLE_GC(iolist_to_iovec, 1)

BIF_RETTYPE iolist_to_iovec_1(BIF_ALIST_1) {
    BIF_RETTYPE result;

    if (is_nil(BIF_ARG_1)) {
        BIF_RET(NIL);
    } else if (is_binary(BIF_ARG_1)) {
        if (binary_bitsize(BIF_ARG_1) != 0) {
            ASSERT(!(BIF_P->flags & F_DISABLE_GC));
            BIF_ERROR(BIF_P, BADARG);
        } else if (binary_size(BIF_ARG_1) != 0) {
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
