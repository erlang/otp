/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2017-2025. All Rights Reserved.
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

Uint erts_ioq_size(const ErtsIOQueue *q)
{
    return q->size;
}

/* expand queue to hold n elements in tail or head */
static int expandq(ErtsIOQueue *q, int n, int tail)
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

Uint erts_ioq_peekqv(const ErtsIOQueue *q, ErtsIOVec *ev) {
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

SysIOVec* erts_ioq_peekq(const ErtsIOQueue *q, int* vlenp) /* length of io-vector */
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
