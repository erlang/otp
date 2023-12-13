/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2023. All Rights Reserved.
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
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "bif.h"
#include "erl_binary.h"
#include "big.h"
#include "zlib.h"
#include "erl_md5.h"


typedef void (*ChksumFun)(void *sum_in_out, const byte *buf,
			  unsigned buflen);

/* Hidden trap target */
static BIF_RETTYPE md5_2(BIF_ALIST_2);

static Export chksum_md5_2_exp;

void erts_init_bif_chksum(void)
{
    erts_init_trap_export(&chksum_md5_2_exp,
                          am_erlang, ERTS_MAKE_AM("md5_trap"), 2,
                          &md5_2);
}

static Eterm do_chksum(ChksumFun sumfun, Process *p, Eterm ioterm, int left, 
		       void *sum, int *res, int *err)
{
    Eterm *objp;
    Eterm obj;
    int c;
    DECLARE_ESTACK(stack);
    int numbytes = 0;
    byte *buf = NULL;

    *err = 0;
    if (left <= 0 || is_nil(ioterm)) {
        DESTROY_ESTACK(stack);
        *res = 0;
        return ioterm;
    } else if (is_bitstring(ioterm)) {
        Eterm res_term = NIL;
        const byte *temp_alloc = NULL, *bytes;
        Uint size;

        /* As we've already checked that this is a bitstring, this can only
         * fail when we've got a non-binary. */
        bytes = erts_get_aligned_binary_bytes(ioterm, &size, &temp_alloc);
        if (bytes == NULL) {
            *res = 0;
            *err = 1;
            DESTROY_ESTACK(stack);
            return NIL;
        }

        if (size > left) {
            res_term = erts_make_sub_binary(p, ioterm, left, (size - left));
            size = left;
        }

        (*sumfun)(sum, bytes, size);
        *res = size;

        DESTROY_ESTACK(stack);
        erts_free_aligned_binary_bytes(temp_alloc);
        return res_term;
    }

    if (!is_list(ioterm)) {
        *res = 0;
        *err = 1;
        DESTROY_ESTACK(stack);
        return NIL;
    }

    /* OK a list, needs to be processed in order, handling each flat list-level
       as they occur, just like io_list_to_binary would */
    *res = 0;
    ESTACK_PUSH(stack,ioterm);
    while (!ESTACK_ISEMPTY(stack) && left) {
	ioterm = ESTACK_POP(stack);
	if (is_nil(ioterm)) {
	    /* ignore empty lists */
	    continue;
	}
	if(is_list(ioterm)) {
L_Again:   /* Restart with sublist, old listend was pushed on stack */
	    objp = list_val(ioterm);
	    obj = CAR(objp);
	    for(;;) { /* loop over one flat list of bytes and binaries
		         until sublist or list end is encountered */
		if (is_byte(obj)) {
		    int bsize = 0;
		    for(;;) {
			if (bsize >= numbytes) {
			    if (!buf) {
                                numbytes = 500;
                                buf = erts_alloc(ERTS_ALC_T_TMP, numbytes);
			    } else {
				if (numbytes > left) {
				    numbytes += left;
				} else {
				    numbytes *= 2;
				}
                                buf = erts_realloc(ERTS_ALC_T_TMP, buf,
                                                    numbytes);
			    }
			}  
			buf[bsize++] = (unsigned char) unsigned_val(obj);
			--left;
			ioterm = CDR(objp);
			if (!is_list(ioterm)) {
			    break;
			}
			objp = list_val(ioterm);
			obj = CAR(objp);
			if (!is_byte(obj))
			    break;
			if (!left) {
			    break;
			}
		    }
		    (*sumfun)(sum, buf, bsize);
		    *res += bsize;
		} else if (is_nil(obj)) {
		    ioterm = CDR(objp);
		    if (!is_list(ioterm)) {
			break;
		    }
		    objp = list_val(ioterm);
		    obj = CAR(objp);
		} else if (is_list(obj)) {
		    /* push rest of list for later processing, start 
		       again with sublist */
		    ESTACK_PUSH(stack,CDR(objp));
		    ioterm = obj;
		    goto L_Again;
		} else if (is_bitstring(obj)) {
		    int sres, serr;
		    Eterm rest_term;
		    rest_term = do_chksum(sumfun, p, obj, left, sum, &sres,
					  &serr);
		    *res += sres;
		    if (serr != 0) {
			*err = 1;
			DESTROY_ESTACK(stack);
			if (buf != NULL)
			    erts_free(ERTS_ALC_T_TMP, buf);
			return NIL;
		    }
		    left -= sres;
		    if (rest_term != NIL) {
			Eterm *hp;
			hp = HAlloc(p, 2);
			obj = CDR(objp);
			ioterm = CONS(hp, rest_term, obj);
			left = 0;
			break;
		    }
		    ioterm = CDR(objp);
		    if (is_list(ioterm)) {
			/* objp and obj need to be updated if 
			   loop is to continue */
			objp = list_val(ioterm);
			obj = CAR(objp);
		    }
		} else {
		    *err = 1;
		    DESTROY_ESTACK(stack);
		    if (buf != NULL)
			erts_free(ERTS_ALC_T_TMP, buf);
		    return NIL;
		} 
		if (!left || is_nil(ioterm) || !is_list(ioterm)) {
		    break;
		}
	    } /* for(;;) */
	} /* is_list(ioterm) */

	if (!left) {
#ifdef ALLOW_BYTE_TAIL
	    if (is_byte(ioterm)) {
		/* improper list with byte tail*/
		Eterm *hp;
		hp = HAlloc(p, 2);
		ioterm = CONS(hp, ioterm, NIL);
	    }
#else
	    ;
#endif
	} else if (!is_list(ioterm) && !is_nil(ioterm)) {
	    /* improper list end */
#ifdef ALLOW_BYTE_TAIL
	    if (is_byte(ioterm)) {
		unsigned char b[1];
		b[0] = (unsigned char) unsigned_val(ioterm);
		(*sumfun)(sum, b, 1);
		++(*res);
		--left;
		ioterm = NIL;
	    } else
#endif 
	    if (is_bitstring(ioterm)) {
		int sres, serr;
		ioterm = do_chksum(sumfun, p, ioterm, left, sum, &sres, &serr);
		*res +=sres;
		if (serr != 0) {
		    *err = 1;
		    DESTROY_ESTACK(stack);
		    if (buf != NULL)
			erts_free(ERTS_ALC_T_TMP, buf);
		    return NIL;
		}
		left -= sres;
	    } else {
		*err = 1;
		DESTROY_ESTACK(stack);
		if (buf != NULL)
		    erts_free(ERTS_ALC_T_TMP, buf);
		return NIL;
	    }
	}
    } /* while left and not estack empty */
    c = ESTACK_COUNT(stack);
    if (c > 0) {
	Eterm *hp = HAlloc(p,2*c);
	while(!ESTACK_ISEMPTY(stack)) {
	    Eterm st = ESTACK_POP(stack);
	    ioterm = CONS(hp, ioterm, st);
	    hp += 2;
	}
    }
    DESTROY_ESTACK(stack);
    if (buf != NULL)
	erts_free(ERTS_ALC_T_TMP, buf);
    return ioterm;
}

static void adler32_wrap(void *vsum, const byte *buf, unsigned buflen)
{
    unsigned long sum = *((unsigned long *) vsum);
    sum = adler32(sum,buf,buflen);
    *((unsigned long *) vsum) = sum;
}

static void crc32_wrap(void *vsum, const byte *buf, unsigned buflen)
{
    unsigned long sum = *((unsigned long *) vsum);
    sum = crc32(sum,buf,buflen);
    *((unsigned long *) vsum) = sum;
}

static void md5_wrap(void *vsum, const byte *buf, unsigned buflen)
{
    MD5_CTX *ctx = ((MD5_CTX *) vsum);
    MD5Update(ctx, (unsigned char*)buf, buflen);
}

#define BYTES_PER_REDUCTION 10
#define CHUNK_PER_SCHEDULE (BYTES_PER_REDUCTION * CONTEXT_REDS)

BIF_RETTYPE
crc32_1(BIF_ALIST_1)
{
    unsigned long chksum;
    int res, err;
    Eterm rest,res_sum;
    chksum = crc32(0,NULL,0);

    rest = do_chksum(&crc32_wrap,BIF_P,BIF_ARG_1,CHUNK_PER_SCHEDULE,
		     (void *) &chksum,&res,
		     &err);
    BUMP_REDS(BIF_P,res / BYTES_PER_REDUCTION);
    if (err != 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    res_sum = erts_make_integer(chksum,BIF_P);
    if (rest != NIL) {
	BUMP_ALL_REDS(BIF_P);
	BIF_TRAP2(BIF_TRAP_EXPORT(BIF_crc32_2), BIF_P, res_sum, rest);
    }
    BIF_RET(res_sum);
}

BIF_RETTYPE
crc32_2(BIF_ALIST_2)
{
    unsigned long chksum;
    int res, err;
    Eterm rest,res_sum;
    Uint u;
    if (!term_to_Uint(BIF_ARG_1, &u) || ((u >> 16) >> 16) != 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    chksum = (unsigned long) u;

    rest = do_chksum(&crc32_wrap,BIF_P,BIF_ARG_2,CHUNK_PER_SCHEDULE,
		     (void *) &chksum,&res,
		     &err);
    BUMP_REDS(BIF_P,res / BYTES_PER_REDUCTION);
    if (err != 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    res_sum = erts_make_integer(chksum,BIF_P);
    if (rest != NIL) {
	BUMP_ALL_REDS(BIF_P);
	BIF_TRAP2(BIF_TRAP_EXPORT(BIF_crc32_2), BIF_P, res_sum, rest);
    }
    BIF_RET(res_sum);
}

BIF_RETTYPE
crc32_combine_3(BIF_ALIST_3)
{
    unsigned long chksum1,chksum2;
    z_off_t length;
    Uint32 res;
    Eterm res_sum;
    Uint u;

    if (!term_to_Uint(BIF_ARG_1, &u) || ((u >> 16) >> 16) != 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    chksum1 = (unsigned long) u;

    if (!term_to_Uint(BIF_ARG_2, &u) || ((u >> 16) >> 16) != 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    chksum2 = (unsigned long) u;

    if (!term_to_Uint(BIF_ARG_3, &u) || ((u >> 16) >> 16) != 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    length = (z_off_t) u;

    res = (Uint32) crc32_combine(chksum1,chksum2,length);

    res_sum = erts_make_integer(res,BIF_P);
    BIF_RET(res_sum);
}

BIF_RETTYPE
adler32_1(BIF_ALIST_1)
{
    unsigned long chksum;
    int res, err;
    Eterm rest,res_sum;
    chksum = adler32(0,NULL,0);

    rest = do_chksum(&adler32_wrap,BIF_P,BIF_ARG_1,CHUNK_PER_SCHEDULE,
		     (void *) &chksum,&res,
		     &err);
    BUMP_REDS(BIF_P,res / BYTES_PER_REDUCTION);
    if (err != 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    res_sum = erts_make_integer(chksum,BIF_P);
    if (rest != NIL) {
	BUMP_ALL_REDS(BIF_P);
	BIF_TRAP2(BIF_TRAP_EXPORT(BIF_adler32_2), BIF_P, res_sum, rest);
    }
    BIF_RET(res_sum);
}

BIF_RETTYPE
adler32_2(BIF_ALIST_2)
{
    unsigned long chksum;
    int res, err;
    Eterm rest,res_sum;
    Uint u;
    if (!term_to_Uint(BIF_ARG_1, &u) || ((u >> 16) >> 16) != 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    chksum = (unsigned long) u;

    rest = do_chksum(&adler32_wrap,BIF_P,BIF_ARG_2,CHUNK_PER_SCHEDULE,
		     (void *) &chksum,&res,
		     &err);
    BUMP_REDS(BIF_P,res / BYTES_PER_REDUCTION);
    if (err != 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    res_sum = erts_make_integer(chksum,BIF_P);
    if (rest != NIL) {
	BUMP_ALL_REDS(BIF_P);
	BIF_TRAP2(BIF_TRAP_EXPORT(BIF_adler32_2), BIF_P, res_sum, rest);
    }
    BIF_RET(res_sum);
}

BIF_RETTYPE
adler32_combine_3(BIF_ALIST_3)
{
    unsigned long chksum1,chksum2;
    z_off_t length;
    Uint32 res;
    Eterm res_sum;
    Uint u;

    if (!term_to_Uint(BIF_ARG_1, &u) || ((u >> 16) >> 16) != 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    chksum1 = (unsigned long) u;

    if (!term_to_Uint(BIF_ARG_2, &u) || ((u >> 16) >> 16) != 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    chksum2 = (unsigned long) u;

    if (!term_to_Uint(BIF_ARG_3, &u) || ((u >> 16) >> 16) != 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    length = (z_off_t) u;

    if (length == 0) { /* Workaround for unexpected behaviour in zlib. */
	res = (Uint32) chksum1;
    } else {
	res = (Uint32) adler32_combine(chksum1,chksum2,length);
    }

    res_sum = erts_make_integer(res,BIF_P);
    BIF_RET(res_sum);
}


BIF_RETTYPE
md5_1(BIF_ALIST_1)
{
    Eterm bin, rest;
    int res, err;

    MD5_CTX context;
    MD5Init(&context);

    rest = do_chksum(&md5_wrap,BIF_P,BIF_ARG_1,100,(void *) &context,&res,
                     &err);

    if (err != 0) {
        BUMP_REDS(BIF_P,res);
        BIF_ERROR(BIF_P, BADARG);
    }

    if (rest != NIL) {
        BUMP_ALL_REDS(BIF_P);

        bin = erts_new_binary_from_data(BIF_P, sizeof(MD5_CTX), (byte*)&context);

        BIF_TRAP2(&chksum_md5_2_exp, BIF_P, bin, rest);
    } else {
        byte checksum[MD5_SIZE];

        BUMP_REDS(BIF_P, res);
        MD5Final(checksum, &context);

        return erts_new_binary_from_data(BIF_P, MD5_SIZE, checksum);
    }
}

/* Hidden trap target */
static BIF_RETTYPE
md5_2(BIF_ALIST_2)
{
    Uint offset, size;
    MD5_CTX context;
    byte *bytes;
    Eterm rest;
    Eterm bin;
    int res, err;

    /* No need to check context, this function cannot be called with unaligned
     * or badly sized context as it's always trapped to. */
    ERTS_GET_BITSTRING(BIF_ARG_1, bytes, offset, size);

    ASSERT((offset == 0) && (size == NBITS(sizeof(MD5_CTX))));
    (void)offset;
    (void)size;

    sys_memcpy(&context, bytes, sizeof(MD5_CTX));
    rest = do_chksum(&md5_wrap, BIF_P, BIF_ARG_2, 100, (void*)&context, &res,
                     &err);

    if (err != 0) {
        BUMP_REDS(BIF_P,res);
        BIF_ERROR(BIF_P, BADARG);
    }

    if (rest != NIL) {
        BUMP_ALL_REDS(BIF_P);

        bin = erts_new_binary_from_data(BIF_P, sizeof(MD5_CTX), (byte*)&context);

        BIF_TRAP2(&chksum_md5_2_exp, BIF_P, bin, rest);
    } else {
        byte checksum[MD5_SIZE];

        BUMP_REDS(BIF_P, res);
        MD5Final(checksum, &context);

        return erts_new_binary_from_data(BIF_P, MD5_SIZE, checksum);
    }
}

BIF_RETTYPE
md5_init_0(BIF_ALIST_0)
{
    Eterm bin;
    byte* bytes;

    bin = erts_new_binary(BIF_P, sizeof(MD5_CTX), &bytes);
    MD5Init((MD5_CTX*)bytes);

    BIF_RET(bin);
}

BIF_RETTYPE
md5_update_2(BIF_ALIST_2)
{
    const byte *temp_alloc = NULL, *bytes;
    MD5_CTX *context;
    Eterm rest;
    Eterm bin;
    int res, err;
    Uint size;

    bytes = erts_get_aligned_binary_bytes(BIF_ARG_1, &size, &temp_alloc);
    if (bytes == NULL || size != sizeof(MD5_CTX)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    bin = erts_new_binary(BIF_P, sizeof(MD5_CTX), (byte**)&context);
    sys_memcpy(context, bytes, sizeof(MD5_CTX));

    erts_free_aligned_binary_bytes(temp_alloc);

    rest = do_chksum(&md5_wrap, BIF_P, BIF_ARG_2, 100, (void *)context, &res,
                     &err);

    if (err != 0) {
        BUMP_REDS(BIF_P, res);
        BIF_ERROR(BIF_P, BADARG);
    }

    if (rest != NIL) {
        BUMP_ALL_REDS(BIF_P);
        BIF_TRAP2(BIF_TRAP_EXPORT(BIF_md5_update_2), BIF_P, bin, rest);
    }

    BUMP_REDS(BIF_P,res);
    BIF_RET(bin);
}

BIF_RETTYPE
md5_final_1(BIF_ALIST_1)
{
    const byte *temp_alloc = NULL, *context;
    MD5_CTX ctx_copy;
    byte* result;
    Uint size;
    Eterm bin;

    context = erts_get_aligned_binary_bytes(BIF_ARG_1, &size, &temp_alloc);
    if (context == NULL || size != sizeof(MD5_CTX)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    sys_memcpy(&ctx_copy, context, sizeof(MD5_CTX));
    erts_free_aligned_binary_bytes(temp_alloc);

    bin = erts_new_binary(BIF_P, MD5_SIZE, &result);
    MD5Final(result, &ctx_copy);

    BIF_RET(bin);
}
