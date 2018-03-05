/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2016. All Rights Reserved.
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


typedef void (*ChksumFun)(void *sum_in_out, unsigned char *buf, 
			  unsigned buflen);

/* Hidden trap target */
static BIF_RETTYPE md5_2(BIF_ALIST_2);

static Export chksum_md5_2_exp;

void erts_init_bif_chksum(void)
{
    /* Non visual BIF to trap to. */
    erts_init_trap_export(&chksum_md5_2_exp,
			  am_erlang, am_atom_put("md5_trap",8), 2,
			  &md5_2);
}
    

static Eterm do_chksum(ChksumFun sumfun, Process *p, Eterm ioterm, int left, 
		       void *sum, int *res, int *err)
{
    Eterm *objp;
    Eterm obj;
    int c;
    DECLARE_ESTACK(stack);
    unsigned char *bytes = NULL;
    int numbytes = 0;

    *err = 0;
    if (left <= 0 || is_nil(ioterm)) {
	DESTROY_ESTACK(stack);
	*res = 0;
	return ioterm;
    }
    if(is_binary(ioterm)) {
	Uint bitoffs;
	Uint bitsize;
	Uint size;
	Eterm res_term = NIL;
	unsigned char *bytes;
	byte *temp_alloc = NULL;
	
	ERTS_GET_BINARY_BYTES(ioterm, bytes, bitoffs, bitsize);
	if (bitsize != 0) {
	    *res = 0;
	    *err = 1;
	    DESTROY_ESTACK(stack);
	    return NIL;
	}
	if (bitoffs != 0) {
	    bytes = erts_get_aligned_binary_bytes(ioterm, &temp_alloc);
	    /* The call to erts_get_aligned_binary_bytes cannot fail as 
	       we'we already checked bitsize and that this is a binary */
	}

	size = binary_size(ioterm);


	if (size > left) {
	    Eterm *hp;
	    ErlSubBin *sb;
	    Eterm orig;
	    Uint offset;
	    /* Split the binary in two parts, of which we 
	       only process the first */
	    hp = HAlloc(p, ERL_SUB_BIN_SIZE);
	    sb = (ErlSubBin *) hp;
	    ERTS_GET_REAL_BIN(ioterm, orig, offset, bitoffs, bitsize);
	    sb->thing_word = HEADER_SUB_BIN;
	    sb->size = size - left;
	    sb->offs = offset + left;
	    sb->orig = orig;
	    sb->bitoffs = bitoffs;
	    sb->bitsize = bitsize;
	    sb->is_writable = 0;
	    res_term = make_binary(sb);
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
			    if (!bytes) {
				bytes = erts_alloc(ERTS_ALC_T_TMP, 
						   numbytes = 500);
			    } else {
				if (numbytes > left) {
				    numbytes += left;
				} else {
				    numbytes *= 2;
				}
				bytes = erts_realloc(ERTS_ALC_T_TMP, bytes,
						     numbytes);
			    }
			}  
			bytes[bsize++] = (unsigned char) unsigned_val(obj);
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
		    (*sumfun)(sum, bytes, bsize);
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
		} else if (is_binary(obj)) {
		    int sres, serr;
		    Eterm rest_term;
		    rest_term = do_chksum(sumfun, p, obj, left, sum, &sres,
					  &serr);
		    *res += sres;
		    if (serr != 0) {
			*err = 1;
			DESTROY_ESTACK(stack);
			if (bytes != NULL)
			    erts_free(ERTS_ALC_T_TMP, bytes);
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
		    if (bytes != NULL)
			erts_free(ERTS_ALC_T_TMP, bytes);
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
		/* inproper list with byte tail*/
		Eterm *hp;
		hp = HAlloc(p, 2);
		ioterm = CONS(hp, ioterm, NIL);
	    }
#else
	    ;
#endif
	} else if (!is_list(ioterm) && !is_nil(ioterm)) {
	    /* inproper list end */
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
	    if is_binary(ioterm) {
		int sres, serr;
		ioterm = do_chksum(sumfun, p, ioterm, left, sum, &sres, &serr);
		*res +=sres;
		if (serr != 0) {
		    *err = 1;
		    DESTROY_ESTACK(stack);
		    if (bytes != NULL)
			erts_free(ERTS_ALC_T_TMP, bytes);
		    return NIL;
		}
		left -= sres;
	    } else {
		*err = 1;
		DESTROY_ESTACK(stack);
		if (bytes != NULL)
		    erts_free(ERTS_ALC_T_TMP, bytes);
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
    if (bytes != NULL)
	erts_free(ERTS_ALC_T_TMP, bytes);
    return ioterm;
}

static void adler32_wrap(void *vsum, unsigned char *buf, unsigned buflen)
{
    unsigned long sum = *((unsigned long *) vsum);
    sum = adler32(sum,buf,buflen);
    *((unsigned long *) vsum) = sum;
}

static void crc32_wrap(void *vsum, unsigned char *buf, unsigned buflen)
{
    unsigned long sum = *((unsigned long *) vsum);
    sum = crc32(sum,buf,buflen);
    *((unsigned long *) vsum) = sum;
}

static void md5_wrap(void *vsum, unsigned char *buf, unsigned buflen)
{
    MD5_CTX *ctx = ((MD5_CTX *) vsum);
    MD5Update(ctx,buf,buflen);
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
	BIF_TRAP2(bif_export[BIF_crc32_2], BIF_P, res_sum, rest);
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
	BIF_TRAP2(bif_export[BIF_crc32_2], BIF_P, res_sum, rest);
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
	BIF_TRAP2(bif_export[BIF_adler32_2], BIF_P, res_sum, rest);
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
	BIF_TRAP2(bif_export[BIF_adler32_2], BIF_P, res_sum, rest);
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
    Eterm bin;
    byte* bytes;
    Eterm rest;
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
	 bin = new_binary(BIF_P, (byte *) &context, sizeof(MD5_CTX));
	 BIF_TRAP2(&chksum_md5_2_exp, BIF_P, bin, rest);
    }
    BUMP_REDS(BIF_P,res);
    bin = new_binary(BIF_P, (byte *)NULL, 16);
    bytes = binary_bytes(bin);
    MD5Final(bytes, &context);
    BIF_RET(bin);
}

/* Hidden trap target */
static BIF_RETTYPE
md5_2(BIF_ALIST_2)
{
    byte *bytes;
    MD5_CTX context;
    Eterm rest;
    Eterm bin;
    int res, err;

    /* No need to check context, this function cannot be called with unaligned
       or badly sized context as it's always trapped to. */
    bytes = binary_bytes(BIF_ARG_1);
    sys_memcpy(&context,bytes,sizeof(MD5_CTX));
    rest = do_chksum(&md5_wrap,BIF_P,BIF_ARG_2,100,(void *) &context,&res,
		     &err);
    if (err != 0) {
	BUMP_REDS(BIF_P,res);
	BIF_ERROR(BIF_P, BADARG);
    }
    if (rest != NIL) {
	BUMP_ALL_REDS(BIF_P);
	bin = new_binary(BIF_P, (byte *) &context, sizeof(MD5_CTX));
	BIF_TRAP2(&chksum_md5_2_exp, BIF_P, bin, rest);
    }
    BUMP_REDS(BIF_P,res);
    bin = new_binary(BIF_P, (byte *)NULL, 16);
    bytes = binary_bytes(bin);
    MD5Final(bytes, &context);
    BIF_RET(bin);
}

BIF_RETTYPE
md5_init_0(BIF_ALIST_0)
{
    Eterm bin;
    byte* bytes;

    bin = erts_new_heap_binary(BIF_P, (byte *)NULL, sizeof(MD5_CTX), &bytes);
    MD5Init((MD5_CTX *)bytes);
    BIF_RET(bin);
}

BIF_RETTYPE
md5_update_2(BIF_ALIST_2)
{
    byte *bytes;
    MD5_CTX context;
    Eterm rest;
    Eterm bin;
    int res, err;
    byte *temp_alloc = NULL;

    if ((bytes = erts_get_aligned_binary_bytes(BIF_ARG_1, &temp_alloc)) == NULL) {
	erts_free_aligned_binary_bytes(temp_alloc);
	BIF_ERROR(BIF_P, BADARG);
    }
    if (binary_size(BIF_ARG_1) != sizeof(MD5_CTX)) {
	erts_free_aligned_binary_bytes(temp_alloc);
	BIF_ERROR(BIF_P, BADARG);
    }
    sys_memcpy(&context,bytes,sizeof(MD5_CTX));
    erts_free_aligned_binary_bytes(temp_alloc);
    rest = do_chksum(&md5_wrap,BIF_P,BIF_ARG_2,100,(void *) &context,&res,
		     &err);
    if (err != 0) {
	BUMP_REDS(BIF_P,res);
	BIF_ERROR(BIF_P, BADARG);
    }
    bin = new_binary(BIF_P, (byte *) &context, sizeof(MD5_CTX));
    if (rest != NIL) {
	BUMP_ALL_REDS(BIF_P);
	BIF_TRAP2(bif_export[BIF_md5_update_2], BIF_P, bin, rest);
    }
    BUMP_REDS(BIF_P,res);
    BIF_RET(bin);
}

BIF_RETTYPE
md5_final_1(BIF_ALIST_1)
{
    Eterm bin;
    byte* context;
    byte* result;
    MD5_CTX ctx_copy;
    byte* temp_alloc = NULL;

    if ((context = erts_get_aligned_binary_bytes(BIF_ARG_1, &temp_alloc)) == NULL) {
    error:
	erts_free_aligned_binary_bytes(temp_alloc);
	BIF_ERROR(BIF_P, BADARG);
    }
    if (binary_size(BIF_ARG_1) != sizeof(MD5_CTX)) {
	goto error;
    }
    bin = erts_new_heap_binary(BIF_P, (byte *)NULL, 16, &result);
    sys_memcpy(&ctx_copy, context, sizeof(MD5_CTX));
    erts_free_aligned_binary_bytes(temp_alloc);
    MD5Final(result, &ctx_copy);
    BIF_RET(bin);
}
