/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2017. All Rights Reserved.
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
 *
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <erl_nif.h>

/* #define ASN1_DEBUG 1 */

#define ASN1_OK 0
#define ASN1_ERROR -1
#define ASN1_COMPL_ERROR 1
#define ASN1_DECODE_ERROR 2
#define ASN1_TAG_ERROR -3
#define ASN1_LEN_ERROR -4
#define ASN1_INDEF_LEN_ERROR -5
#define ASN1_VALUE_ERROR -6

#define ASN1_CLASS 0xc0
#define ASN1_FORM 0x20
#define ASN1_CLASSFORM (ASN1_CLASS | ASN1_FORM)
#define ASN1_TAG 0x1f
#define ASN1_LONG_TAG 0x7f

#define ASN1_INDEFINITE_LENGTH 0x80
#define ASN1_SHORT_DEFINITE_LENGTH 0

#define ASN1_PRIMITIVE 0
#define ASN1_CONSTRUCTED 0x20

#define ASN1_NOVALUE 0

#define ASN1_SKIPPED 0
#define ASN1_OPTIONAL 1
#define ASN1_CHOOSEN 2

#define CEIL(X,Y) ((X-1) / Y + 1)

#define INVMASK(X,M) (X & (M ^ 0xff))
#define MASK(X,M) (X & M)

/* PER COMPLETE */
static int per_complete(ErlNifBinary *, unsigned char *, int);

static int per_insert_octets(int, unsigned char **, unsigned char **, int *);

static int per_insert_octets_except_unused(int, unsigned char **, unsigned char **,
	int *, int);

static int per_insert_octets_as_bits_exact_len(int, int, unsigned char **,
	unsigned char **, int *);

static int per_insert_octets_as_bits(int, unsigned char **, unsigned char **, int *);

static int per_pad_bits(int, unsigned char **, int *);

static int per_insert_least_sign_bits(int, unsigned char, unsigned char **, int *);

static int per_insert_most_sign_bits(int, unsigned char, unsigned char **, int *);

static int per_insert_bits_as_bits(int, int, unsigned char **, unsigned char **, int *);

static int per_insert_octets_unaligned(int, unsigned char **, unsigned char **, int);

static int per_realloc_memory(ErlNifBinary *, int, unsigned char **);

/* BER DECODE */
static int ber_decode_begin(ErlNifEnv *, ERL_NIF_TERM *, unsigned char *, int,
	unsigned int *);

static int ber_decode(ErlNifEnv *, ERL_NIF_TERM *, unsigned char *, int *, int);

static int ber_decode_tag(ErlNifEnv *, ERL_NIF_TERM *, unsigned char *, int, int *);

static int ber_decode_value(ErlNifEnv*, ERL_NIF_TERM *, unsigned char *, int *, int,
	int);

/* BER ENCODE */
typedef struct ber_encode_mem_chunk mem_chunk_t;

static int ber_encode(ErlNifEnv *, ERL_NIF_TERM , mem_chunk_t **, unsigned int *);

static void ber_free_chunks(mem_chunk_t *chunk);
static mem_chunk_t *ber_new_chunk(unsigned int length);
static int ber_check_memory(mem_chunk_t **curr, unsigned int needed);

static int ber_encode_tag(ErlNifEnv *, ERL_NIF_TERM , unsigned int ,
	mem_chunk_t **, unsigned int *);

static int ber_encode_length(size_t , mem_chunk_t **, unsigned int *);

/*
 *
 * This section defines functionality for the complete encode of a
 * PER encoded message
 *
 */

static int per_complete(ErlNifBinary *out_binary, unsigned char *in_buf,
	int in_buf_len) {
    int counter = in_buf_len;
    /* counter keeps track of number of bytes left in the
     input buffer */

    int buf_space = in_buf_len;
    /* This is the amount of allocated space left of the out_binary. It
     is possible when padding is applied that more space is needed than
     was originally allocated. */

    int buf_size = in_buf_len;
    /* Size of the buffer. May become reallocated and thus other than
     in_buf_len */

    unsigned char *in_ptr, *ptr;
    /* in_ptr points at the next byte in in_buf to be moved to
     complete_buf.
     ptr points into the new completed buffer, complete_buf, at the
     position of the next byte that will be set */
    int unused = 8;
    /* unused = [1,...,8] indicates how many of the rigthmost bits of
     the byte that ptr points at that are unassigned */

    int no_bits, no_bytes, in_unused, desired_len, ret, saved_mem, needed,
	    pad_bits;

    unsigned char val;

    in_ptr = in_buf;
    ptr = out_binary->data;
    *ptr = 0x00;
    while (counter > 0) {
	counter--;
	switch (*in_ptr) {
	case 0:
	    /* just one zero-bit should be added to the buffer */
	    if (unused == 1) {
		unused = 8;
		*++ptr = 0x00;
		buf_space--;
	    } else
		unused--;
	    break;

	case 1:
	    /* one one-bit should be added to the buffer */
	    if (unused == 1) {
		*ptr = *ptr | 1;
		unused = 8;
		*++ptr = 0x00;
		buf_space--;
	    } else {
		*ptr = *ptr | (1 << (unused - 1));
		unused--;
	    }
	    break;

	case 2:
	    /* align buffer to end of byte */
	    if (unused != 8) {
		*++ptr = 0x00;
		buf_space--;
		unused = 8;
	    }
	    break;

	case 10:
	    /* next byte in in_buf tells how many bits in the second next
	     byte that will be used */
	    /* The leftmost unused bits in the value byte are supposed to be
	     zero bits */
	    no_bits = (int) *(++in_ptr);
	    val = *(++in_ptr);
	    counter -= 2;
	    if ((ret = per_insert_least_sign_bits(no_bits, val, &ptr, &unused))
		    == ASN1_ERROR
		    )
		return ASN1_ERROR;
	    buf_space -= ret;
	    break;

	case 20:
	    /* in this case the next value in_ptr points at holds the number
	     of following bytes that holds the value that will be inserted
	     in the completed buffer */
	    no_bytes = (int) *(++in_ptr);
	    counter -= (no_bytes + 1);
	    if ((counter < 0)
		    || (ret = per_insert_octets(no_bytes, &in_ptr, &ptr,
			    &unused)) == ASN1_ERROR
		    )
		return ASN1_ERROR;
	    buf_space -= ret;
	    break;

	case 21:
	    /* in this case the next two bytes in_ptr points at holds the number
	     of following bytes that holds the value that will be inserted
	     in the completed buffer */
	    no_bytes = (int) *(++in_ptr);
	    no_bytes = no_bytes << 8;
	    no_bytes = no_bytes | (int) *(++in_ptr);
	    counter -= (2 + no_bytes);
	    if ((counter < 0)
		    || (ret = per_insert_octets(no_bytes, &in_ptr, &ptr,
			    &unused)) == ASN1_ERROR
		    )
		return ASN1_ERROR;
	    buf_space -= ret;
	    break;

	case 30:
	    /* If we call the following bytes, in the buffer in_ptr points at,
	     By1,By2,Rest then Rest is the value that will be transfered to
	     the completed buffer. By1 tells how many of the rightmost bits in
	     Rest that should not be used. By2 is the length of Rest in bytes.*/
	    in_unused = (int) *(++in_ptr);
	    no_bytes = (int) *(++in_ptr);
	    counter -= (2 + no_bytes);
	    ret = -4711;
	    if ((counter < 0)
		    || (ret = per_insert_octets_except_unused(no_bytes, &in_ptr,
			    &ptr, &unused, in_unused)) == ASN1_ERROR
		    )
		return ASN1_ERROR;
	    buf_space -= ret;
	    break;

	case 31:
	    /* If we call the following bytes, in the buffer in_ptr points at,
	     By1,By2,By3,Rest then Rest is the value that will be transfered to
	     the completed buffer. By1 tells how many of the rightmost bits in
	     Rest that should not be used. By2 and By3 is the length of
	     Rest in bytes.*/
	    in_unused = (int) *(++in_ptr);
	    no_bytes = (int) *(++in_ptr);
	    no_bytes = no_bytes << 8;
	    no_bytes = no_bytes | (int) *(++in_ptr);
	    counter -= (3 + no_bytes);
	    if ((counter < 0)
		    || (ret = per_insert_octets_except_unused(no_bytes, &in_ptr,
			    &ptr, &unused, in_unused)) == ASN1_ERROR
		    )
		return ASN1_ERROR;
	    buf_space -= ret;
	    break;

	case 40:
	    /* This case implies that next byte,By1,(..,By1,By2,Bin,...)
	     is the desired length of the completed value, maybe needs
	     padding zero bits or removal of trailing zero bits from Bin.
	     By2 is the length of Bin and Bin is the value that will be
	     put into the completed buffer. Each byte in Bin has the value
	     1 or 0.*/
	    desired_len = (int) *(++in_ptr);
	    no_bytes = (int) *(++in_ptr);

	    /* This is the algorithm for need of memory reallocation:
	     Only when padding (cases 40 - 43,45 - 47) more memory may be
	     used than allocated. Therefore one has to keep track of how
	     much of the allocated memory that has been saved, i.e. the
	     difference between the number of parsed bytes of the input buffer
	     and the number of used bytes of the output buffer.
	     If saved memory is less than needed for the padding then we
	     need more memory. */
	    saved_mem = buf_space - counter;
	    pad_bits = desired_len - no_bytes - unused;
	    needed = (pad_bits > 0) ? CEIL(pad_bits,8) : 0;
	    if (saved_mem < needed) {
		/* Have to allocate more memory */
		buf_size += needed;
		buf_space += needed;
		if (per_realloc_memory(out_binary, buf_size, &ptr) == ASN1_ERROR
		)
		    return ASN1_ERROR;
	    }

	    counter -= (2 + no_bytes);
	    if ((counter < 0)
		    || (ret = per_insert_octets_as_bits_exact_len(desired_len,
			    no_bytes, &in_ptr, &ptr, &unused)) == ASN1_ERROR
		    )
		return ASN1_ERROR;
	    buf_space -= ret;
	    break;

	case 41:
	    /* Same as case 40 apart from By2, the length of Bin, which is in
	     two bytes*/
	    desired_len = (int) *(++in_ptr);
	    no_bytes = (int) *(++in_ptr);
	    no_bytes = no_bytes << 8;
	    no_bytes = no_bytes | (int) *(++in_ptr);

	    saved_mem = buf_space - counter;
	    needed = CEIL((desired_len-unused),8) - no_bytes;
	    if (saved_mem < needed) {
		/* Have to allocate more memory */
		buf_size += needed;
		buf_space += needed;
		if (per_realloc_memory(out_binary, buf_size, &ptr) == ASN1_ERROR
		)
		    return ASN1_ERROR;
	    }

	    counter -= (3 + no_bytes);
	    if ((counter < 0)
		    || (ret = per_insert_octets_as_bits_exact_len(desired_len,
			    no_bytes, &in_ptr, &ptr, &unused)) == ASN1_ERROR
		    )
		return ASN1_ERROR;
	    buf_space -= ret;
	    break;

	case 42:
	    /* Same as case 40 apart from By1, the desired length, which is in
	     two bytes*/
	    desired_len = (int) *(++in_ptr);
	    desired_len = desired_len << 8;
	    desired_len = desired_len | (int) *(++in_ptr);
	    no_bytes = (int) *(++in_ptr);

	    saved_mem = buf_space - counter;
	    needed = CEIL((desired_len-unused),8) - no_bytes;
	    if (saved_mem < needed) {
		/* Have to allocate more memory */
		buf_size += needed;
		buf_space += needed;
		if (per_realloc_memory(out_binary, buf_size, &ptr) == ASN1_ERROR
		)
		    return ASN1_ERROR;
	    }

	    counter -= (3 + no_bytes);
	    if ((counter < 0)
		    || (ret = per_insert_octets_as_bits_exact_len(desired_len,
			    no_bytes, &in_ptr, &ptr, &unused)) == ASN1_ERROR
		    )
		return ASN1_ERROR;
	    buf_space -= ret;
	    break;

	case 43:
	    /* Same as case 40 apart from By1 and By2, the desired length and
	     the length of Bin, which are in two bytes each. */
	    desired_len = (int) *(++in_ptr);
	    desired_len = desired_len << 8;
	    desired_len = desired_len | (int) *(++in_ptr);
	    no_bytes = (int) *(++in_ptr);
	    no_bytes = no_bytes << 8;
	    no_bytes = no_bytes | (int) *(++in_ptr);

	    saved_mem = buf_space - counter;
	    needed = CEIL((desired_len-unused),8) - no_bytes;
	    if (saved_mem < needed) {
		/* Have to allocate more memory */
		buf_size += needed;
		buf_space += needed;
		if (per_realloc_memory(out_binary, buf_size, &ptr) == ASN1_ERROR
		)
		    return ASN1_ERROR;
	    }

	    counter -= (4 + no_bytes);
	    if ((counter < 0)
		    || (ret = per_insert_octets_as_bits_exact_len(desired_len,
			    no_bytes, &in_ptr, &ptr, &unused)) == ASN1_ERROR
		    )
		return ASN1_ERROR;
	    buf_space -= ret;
	    break;

	case 45:
	    /* This case assumes that the following bytes in the incoming buffer
	     (called By1,By2,Bin) is By1, which is the number of bits (n) that
	     will be inserted in the completed buffer. By2 is the number of
	     bytes in Bin. Each bit in the buffer Bin should be inserted from
	     the leftmost until the nth.*/
	    desired_len = (int) *(++in_ptr);
	    no_bytes = (int) *(++in_ptr);

	    saved_mem = buf_space - counter;
	    needed = CEIL((desired_len-unused),8) - no_bytes;
	    if (saved_mem < needed) {
		/* Have to allocate more memory */
		buf_size += needed;
		buf_space += needed;
		if (per_realloc_memory(out_binary, buf_size, &ptr) == ASN1_ERROR
		)
		    return ASN1_ERROR;
	    }

	    counter -= (2 + no_bytes);

	    if ((counter < 0)
		    || (ret = per_insert_bits_as_bits(desired_len, no_bytes,
			    &in_ptr, &ptr, &unused)) == ASN1_ERROR
		    )
		return ASN1_ERROR;
	    buf_space -= ret;
	    break;

	case 46:
	    /* Same as case 45 apart from By1, the desired length, which is
	     in two bytes. */
	    desired_len = (int) *(++in_ptr);
	    desired_len = desired_len << 8;
	    desired_len = desired_len | (int) *(++in_ptr);
	    no_bytes = (int) *(++in_ptr);

	    saved_mem = buf_space - counter;
	    needed = CEIL((desired_len-unused),8) - no_bytes;
	    if (saved_mem < needed) {
		/* Have to allocate more memory */
		buf_size += needed;
		buf_space += needed;
		if (per_realloc_memory(out_binary, buf_size, &ptr) == ASN1_ERROR
		)
		    return ASN1_ERROR;
	    }

	    counter -= (3 + no_bytes);
	    if ((counter < 0)
		    || (ret = per_insert_bits_as_bits(desired_len, no_bytes,
			    &in_ptr, &ptr, &unused)) == ASN1_ERROR
		    )
		return ASN1_ERROR;
	    buf_space -= ret;
	    break;

	case 47:
	    /* Same as case 45 apart from By1 and By2, the desired length
	     and the length of Bin, which are in two bytes each. */
	    desired_len = (int) *(++in_ptr);
	    desired_len = desired_len << 8;
	    desired_len = desired_len | (int) *(++in_ptr);
	    no_bytes = (int) *(++in_ptr);
	    no_bytes = no_bytes << 8;
	    no_bytes = no_bytes | (int) *(++in_ptr);

	    saved_mem = buf_space - counter;
	    needed = CEIL((desired_len-unused),8) - no_bytes;
	    if (saved_mem < needed) {
		/* Have to allocate more memory */
		buf_size += needed;
		buf_space += needed;
		if (per_realloc_memory(out_binary, buf_size, &ptr) == ASN1_ERROR
		)
		    return ASN1_ERROR;
	    }

	    counter -= (4 + no_bytes);
	    if ((counter < 0)
		    || (ret = per_insert_bits_as_bits(desired_len, no_bytes,
			    &in_ptr, &ptr, &unused)) == ASN1_ERROR
		    )
		return ASN1_ERROR;
	    buf_space -= ret;
	    break;

	default:
	    return ASN1_ERROR;
	}
	in_ptr++;
    }
    /* The returned buffer must be at least one byte and
     it must be octet aligned */
    if ((unused == 8) && (ptr != out_binary->data))
	return (ptr - out_binary->data);
    else {
	ptr++; /* octet align buffer */
	return (ptr - out_binary->data);
    }
}

static int per_realloc_memory(ErlNifBinary *binary, int amount, unsigned char **ptr) {

    int i = *ptr - binary->data;

    if (!enif_realloc_binary(binary, amount)) {
	/*error handling due to memory allocation failure */
	return ASN1_ERROR;
    } else {
	*ptr = binary->data + i;
    }
    return ASN1_OK;
}

static int per_insert_most_sign_bits(int no_bits, unsigned char val,
	unsigned char **output_ptr, int *unused) {
    unsigned char *ptr = *output_ptr;

    if (no_bits < *unused) {
	*ptr = *ptr | (val >> (8 - *unused));
	*unused -= no_bits;
    } else if (no_bits == *unused) {
	*ptr = *ptr | (val >> (8 - *unused));
	*unused = 8;
	*++ptr = 0x00;
    } else {
	*ptr = *ptr | (val >> (8 - *unused));
	*++ptr = 0x00;
	*ptr = *ptr | (val << *unused);
	*unused = 8 - (no_bits - *unused);
    }
    *output_ptr = ptr;
    return ASN1_OK;
}

static int per_insert_least_sign_bits(int no_bits, unsigned char val,
	unsigned char **output_ptr, int *unused) {
    unsigned char *ptr = *output_ptr;
    int ret = 0;

    if (no_bits < *unused) {
	*ptr = *ptr | (val << (*unused - no_bits));
	*unused -= no_bits;
    } else if (no_bits == *unused) {
	*ptr = *ptr | val;
	*unused = 8;
	*++ptr = 0x00;
	ret++;
    } else {
	/* first in the begun byte in the completed buffer insert
	 so many bits that fit, then insert the rest in next byte.*/
	*ptr = *ptr | (val >> (no_bits - *unused));
	*++ptr = 0x00;
	ret++;
	*ptr = *ptr | (val << (8 - (no_bits - *unused)));
	*unused = 8 - (no_bits - *unused);
    }
    *output_ptr = ptr;
    return ret;
}

/* per_pad_bits adds no_bits bits in the buffer that output_ptr
 points at.
 */
static int per_pad_bits(int no_bits, unsigned char **output_ptr, int *unused) {
    unsigned char *ptr = *output_ptr;
    int ret = 0;

    while (no_bits > 0) {
	if (*unused == 1) {
	    *unused = 8;
	    *++ptr = 0x00;
	    ret++;
	} else
	    (*unused)--;
	no_bits--;
    }
    *output_ptr = ptr;
    return ret;
}

/* insert_bits_as_bits removes no_bytes bytes from the buffer that in_ptr
 points at and takes the desired_no leftmost bits from those removed
 bytes and inserts them in the buffer(output buffer) that ptr points at.
 The unused parameter tells how many bits that are not set in the
 actual byte in the output buffer. If desired_no is more bits than the
 input buffer has in no_bytes bytes, then zero bits is padded.*/
static int per_insert_bits_as_bits(int desired_no, int no_bytes,
	unsigned char **input_ptr, unsigned char **output_ptr, int *unused) {
    unsigned char *in_ptr = *input_ptr;
    unsigned char val;
    int no_bits, ret;

    if (desired_no == (no_bytes * 8)) {
	if (per_insert_octets_unaligned(no_bytes, &in_ptr, output_ptr, *unused)
		== ASN1_ERROR
		)
	    return ASN1_ERROR;
	ret = no_bytes;
    } else if (desired_no < (no_bytes * 8)) {
	/*     printf("per_insert_bits_as_bits 1\n\r"); */
	if (per_insert_octets_unaligned(desired_no / 8, &in_ptr, output_ptr,
		*unused) == ASN1_ERROR
	)
	    return ASN1_ERROR;
	/*     printf("per_insert_bits_as_bits 2\n\r"); */
	val = *++in_ptr;
	/*     printf("val = %d\n\r",(int)val); */
	no_bits = desired_no % 8;
	/*     printf("no_bits = %d\n\r",no_bits); */
	per_insert_most_sign_bits(no_bits, val, output_ptr, unused);
	ret = CEIL(desired_no,8);
    } else {
	if (per_insert_octets_unaligned(no_bytes, &in_ptr, output_ptr, *unused)
		== ASN1_ERROR
		)
	    return ASN1_ERROR;
	per_pad_bits(desired_no - (no_bytes * 8), output_ptr, unused);
	ret = CEIL(desired_no,8);
	/*     printf("ret = %d\n\r",ret); */
    }
    /*   printf("*unused = %d\n\r",*unused); */
    *input_ptr = in_ptr;
    return ret;
}

/* per_insert_octets_as_bits_exact_len */
static int per_insert_octets_as_bits_exact_len(int desired_len, int in_buff_len,
	unsigned char **in_ptr, unsigned char **ptr, int *unused) {
    int ret = 0;
    int ret2 = 0;

    if (desired_len == in_buff_len) {
	if ((ret = per_insert_octets_as_bits(in_buff_len, in_ptr, ptr, unused))
		== ASN1_ERROR
		)
	    return ASN1_ERROR;
    } else if (desired_len > in_buff_len) {
	if ((ret = per_insert_octets_as_bits(in_buff_len, in_ptr, ptr, unused))
		== ASN1_ERROR
		)
	    return ASN1_ERROR;
	/* now pad with zero bits */
	/*     printf("~npad_bits: called with %d bits padding~n~n~r",desired_len - in_buff_len); */
	if ((ret2 = per_pad_bits(desired_len - in_buff_len, ptr, unused))
		== ASN1_ERROR
		)
	    return ASN1_ERROR;
    } else {/* desired_len < no_bits */
	if ((ret = per_insert_octets_as_bits(desired_len, in_ptr, ptr, unused))
		== ASN1_ERROR
		)
	    return ASN1_ERROR;
	/* now remove no_bits - desired_len bytes from in buffer */
	*in_ptr += (in_buff_len - desired_len);
    }
    return (ret + ret2);
}

/* insert_octets_as_bits takes no_bytes bytes from the buffer that input_ptr
 points at and inserts the least significant bit of it in the buffer that
 output_ptr points at. Each byte in the input buffer must be 1 or 0
 otherwise the function returns ASN1_ERROR. The output buffer is concatenated
 without alignment.
 */
static int per_insert_octets_as_bits(int no_bytes, unsigned char **input_ptr,
	unsigned char **output_ptr, int *unused) {
    unsigned char *in_ptr = *input_ptr;
    unsigned char *ptr = *output_ptr;
    int used_bits = 8 - *unused;

    while (no_bytes > 0) {
	switch (*++in_ptr) {
	case 0:
	    if (*unused == 1) {
		*unused = 8;
		*++ptr = 0x00;
	    } else
		(*unused)--;
	    break;
	case 1:
	    if (*unused == 1) {
		*ptr = *ptr | 1;
		*unused = 8;
		*++ptr = 0x00;
	    } else {
		*ptr = *ptr | (1 << (*unused - 1));
		(*unused)--;
	    }
	    break;
	default:
	    return ASN1_ERROR;
	}
	no_bytes--;
    }
    *input_ptr = in_ptr;
    *output_ptr = ptr;
    return ((used_bits + no_bytes) / 8); /*return number of new bytes
     in completed buffer */
}

/* insert_octets inserts bytes from the input buffer, *input_ptr,
 into the output buffer, *output_ptr. Before the first byte is
 inserted the input buffer is aligned.
 */
static int per_insert_octets(int no_bytes, unsigned char **input_ptr,
	unsigned char **output_ptr, int *unused) {
    unsigned char *in_ptr = *input_ptr;
    unsigned char *ptr = *output_ptr;
    int ret = 0;

    if (*unused != 8) {/* must align before octets are added */
	*++ptr = 0x00;
	ret++;
	*unused = 8;
    }
    while (no_bytes > 0) {
	*ptr = *(++in_ptr);
	*++ptr = 0x00;
	/*      *unused = *unused - 1; */
	no_bytes--;
    }
    *input_ptr = in_ptr;
    *output_ptr = ptr;
    return (ret + no_bytes);
}

/* per_insert_octets_unaligned inserts bytes from the input buffer, *input_ptr,
 into the output buffer, *output_ptr.No alignment is done.
 */
static int per_insert_octets_unaligned(int no_bytes, unsigned char **input_ptr,
	unsigned char **output_ptr, int unused) {
    unsigned char *in_ptr = *input_ptr;
    unsigned char *ptr = *output_ptr;
    int n = no_bytes;
    unsigned char val;

    while (n > 0) {
	if (unused == 8) {
	    *ptr = *++in_ptr;
	    *++ptr = 0x00;
	} else {
	    val = *++in_ptr;
	    *ptr = *ptr | val >> (8 - unused);
	    *++ptr = 0x00;
	    *ptr = val << unused;
	}
	n--;
    }
    *input_ptr = in_ptr;
    *output_ptr = ptr;
    return no_bytes;
}

static int per_insert_octets_except_unused(int no_bytes, unsigned char **input_ptr,
	unsigned char **output_ptr, int *unused, int in_unused) {
    unsigned char *in_ptr = *input_ptr;
    unsigned char *ptr = *output_ptr;
    int val, no_bits;
    int ret = 0;

    if (in_unused == 0) {
	if ((ret = per_insert_octets_unaligned(no_bytes, &in_ptr, &ptr, *unused))
		== ASN1_ERROR
		)
	    return ASN1_ERROR;
    } else {
	if ((ret = per_insert_octets_unaligned(no_bytes - 1, &in_ptr, &ptr, *unused))
		!= ASN1_ERROR) {
	    val = (int) *(++in_ptr);
	    no_bits = 8 - in_unused;
	    /* no_bits is always less than *unused since the buffer is
	     octet aligned after insert:octets call, so the following
	     if clasuse is obsolete I think */
	    if (no_bits < *unused) {
		*ptr = *ptr | (val >> (8 - *unused));
		*unused = *unused - no_bits;
	    } else if (no_bits == *unused) {
		*ptr = *ptr | (val >> (8 - *unused));
		*++ptr = 0x00;
		ret++;
		*unused = 8;
	    } else {
		*ptr = *ptr | (val >> (8 - *unused));
		*++ptr = 0x00;
		ret++;
		*ptr = *ptr | (val << *unused);
		*unused = 8 - (no_bits - *unused);
	    }
	} else
	    return ASN1_ERROR;
    }
    *input_ptr = in_ptr;
    *output_ptr = ptr;
    return ret;
}

/*
 *
 * This section defines functionality for the partial decode of a
 * BER encoded message
 *
 */

/*
 * int decode(ErlNifEnv* env, ERL_NIF_TERM *term, unsigned char *in_buf,
 int in_buf_len, unsigned int *err_pos)
 * term is a pointer to the term which is to be returned to erlang
 * in_buf is a pointer into the buffer of incoming bytes.
 * in_buf_len is the length of the incoming buffer.
 * The function reads the bytes in the incoming buffer and structures
 * it in a nested way as Erlang terms. The buffer contains data in the
 * order tag - length - value. Tag, length and value has the following
 * format:
 * A tag is normally one byte but may be of any length, if the tag number
 * is greater than 30. +----------+
 *		       |CL|C|NNNNN|
 * 		       +----------+
 * If NNNNN is 31 then will the 7 l.s.b of each of the following tag number
 * bytes contain the tag number. Each tag number byte that is not the last one
 * has the m.s.b. set to 1.
 * The length can be short definite length (sdl), long definite length (ldl)
 * or indefinite length (il).
 * sdl: +---------+ the L bits is the length
 *	|0|LLLLLLL|
 *	+---------+
 * ldl:	+---------+ +---------+ +---------+     +-----------+
 *	|1|lllllll| |first len| |	  |     |the Nth len|
 *	+---------+ +---------+ +---------+ ... +-----------+
 *    	The first byte tells how many len octets will follow, max 127
 * il:  +---------+ +----------------------+ +--------+ +--------+
 *	|1|0000000| |content octets (Value)| |00000000| |00000000|
 *	+---------+ +----------------------+ +--------+ +--------+
 *	The value octets are preceded by one octet and followed by two
 *	exactly as above. The value must be some tag-length-value encoding.
 *
 * The function returns a value in Erlang nif term format:
 * {{TagNo,Value},Rest}
 * TagNo is an integer ((CL bsl 16) + tag number) which limits the tag number
 * to 65535.
 * Value is a binary if the C bit in tag was unset, otherwise (if tag was
 * constructed) Value is a list, List.
 * List is like: [{TagNo,Value},{TagNo,Value},...]
 * Rest is a binary, i.e. the undecoded part of the buffer. Most often Rest
 * is the empty binary.
 * If some error occured during the decoding of the in_buf an error is returned.
 */
static int ber_decode_begin(ErlNifEnv* env, ERL_NIF_TERM *term, unsigned char *in_buf,
	int in_buf_len, unsigned int *err_pos) {
    int maybe_ret;
    int ib_index = 0;
    unsigned char *rest_data;
    ERL_NIF_TERM decoded_term, rest;

    if ((maybe_ret = ber_decode(env, &decoded_term, in_buf, &ib_index,
	    in_buf_len)) <= ASN1_ERROR)
    {
	*err_pos = ib_index;
	return maybe_ret;
    };

    // The remaining binary after one ASN1 segment has been decoded
    rest_data = enif_make_new_binary(env, in_buf_len - ib_index, &rest);
    memcpy(rest_data, in_buf+ib_index, in_buf_len - ib_index);

    *term = enif_make_tuple2(env, decoded_term, rest);
    return ASN1_OK;
}

static int ber_decode(ErlNifEnv* env, ERL_NIF_TERM *term, unsigned char *in_buf,
	int *ib_index, int in_buf_len) {
    int maybe_ret;
    int form;
    ERL_NIF_TERM tag, value;

    /*buffer must hold at least two bytes*/
    if ((*ib_index + 2) > in_buf_len)
	return ASN1_VALUE_ERROR;
    /* "{{TagNo," */
    if ((form = ber_decode_tag(env, &tag, in_buf, in_buf_len, ib_index))
	    <= ASN1_ERROR
	    )
	return form; /* 5 bytes */
    if (*ib_index >= in_buf_len) {
	return ASN1_TAG_ERROR;
    }
    /* buffer must hold at least one byte (0 as length and nothing as
     value) */
    /* "{{TagNo,Value}," */
    if ((maybe_ret = ber_decode_value(env, &value, in_buf, ib_index, form,
	    in_buf_len)) <= ASN1_ERROR
    )
	return maybe_ret; /* at least 5 bytes */
    *term = enif_make_tuple2(env, tag, value);
    return ASN1_OK;
}

/*
 * decode_tag decodes the BER encoded tag in in_buf and creates an
 * nif term tag
 */
static int ber_decode_tag(ErlNifEnv* env, ERL_NIF_TERM *tag, unsigned char *in_buf,
	int in_buf_len, int *ib_index) {
    int tag_no, tmp_tag, form;

    /* first get the class of tag and bit shift left 16*/
    tag_no = ((MASK(in_buf[*ib_index],ASN1_CLASS)) << 10);

    form = (MASK(in_buf[*ib_index],ASN1_FORM));

    /* then get the tag number */
    if ((tmp_tag = (int) INVMASK(in_buf[*ib_index],ASN1_CLASSFORM)) < 31) {
	*tag = enif_make_uint(env, tag_no | tmp_tag);
	(*ib_index)++;
    } else {
	/* should check that at least three bytes are left in
	 in-buffer,at least two tag byte and at least one length byte */
	if ((*ib_index + 3) > in_buf_len)
	    return ASN1_VALUE_ERROR;
	(*ib_index)++;
	/*
         * The tag is in the following bytes in in_buf as:
         *
         *  1ttttttt 0ttttttt
         *
         * or
         *
         *  0ttttttt
         *
         * where the t-bits is the tag number. If the tag does not
         * fit in two tag bytes (16K), return an error.
         */
	if ((tmp_tag = (int) in_buf[*ib_index]) >= 128) {
	    tag_no = tag_no | (MASK(tmp_tag,ASN1_LONG_TAG) << 7);
	    (*ib_index)++;
	}
        tmp_tag = (int) in_buf[*ib_index];
	if (tmp_tag >= 128) {
	    return ASN1_TAG_ERROR; /* tag number > 16K */
        }
	tag_no = tag_no | tmp_tag;
	(*ib_index)++;
	*tag = enif_make_uint(env, tag_no);
    }
    return form;
}

/*
 * ber_decode_value decodes the BER encoded length and value fields in the
 * in_buf and puts the value part in the decode_buf as an Erlang
 * nif term into value
 */
static int ber_decode_value(ErlNifEnv* env, ERL_NIF_TERM *value, unsigned char *in_buf,
	int *ib_index, int form, int in_buf_len) {
    int maybe_ret;
    unsigned int len = 0;
    unsigned int lenoflen = 0;
    unsigned char *tmp_out_buff;
    ERL_NIF_TERM term = 0, curr_head = 0;

    if (((in_buf[*ib_index]) & 0x80) == ASN1_SHORT_DEFINITE_LENGTH) {
	len = in_buf[*ib_index];
    } else if (in_buf[*ib_index] == ASN1_INDEFINITE_LENGTH) {
	(*ib_index)++;
	curr_head = enif_make_list(env, 0);
	if (*ib_index+1 >= in_buf_len || form == ASN1_PRIMITIVE) {
	    return ASN1_INDEF_LEN_ERROR;
	}
	while (!(in_buf[*ib_index] == 0 && in_buf[*ib_index + 1] == 0)) {
	    maybe_ret = ber_decode(env, &term, in_buf, ib_index, in_buf_len);
	    if (maybe_ret <= ASN1_ERROR) {
		return maybe_ret;
	    }
	    curr_head = enif_make_list_cell(env, term, curr_head);
	    if (*ib_index+1 >= in_buf_len) {
		return ASN1_INDEF_LEN_ERROR;
	    }
	}
	enif_make_reverse_list(env, curr_head, value);
	(*ib_index) += 2; /* skip the indefinite length end bytes */
	return ASN1_OK;
    } else /* long definite length */{
	lenoflen = (in_buf[*ib_index] & 0x7f); /*length of length */
	if (lenoflen > (in_buf_len - (*ib_index + 1)))
	    return ASN1_LEN_ERROR;
	len = 0;
	while (lenoflen--) {
	    (*ib_index)++;
	    if (!(len < (1 << (sizeof(len) - 1) * 8)))
		return ASN1_LEN_ERROR; /* length does not fit in 32 bits */
	    len = (len << 8) + in_buf[*ib_index];
	}
    }
    if (len > (in_buf_len - (*ib_index + 1)))
	return ASN1_VALUE_ERROR;
    (*ib_index)++;
    if (form == ASN1_CONSTRUCTED) {
	int end_index = *ib_index + len;
	if (end_index > in_buf_len)
	    return ASN1_LEN_ERROR;
	curr_head = enif_make_list(env, 0);
	while (*ib_index < end_index) {

	    if ((maybe_ret = ber_decode(env, &term, in_buf, ib_index,
		    in_buf_len)) <= ASN1_ERROR
	    )
		return maybe_ret;
	    curr_head = enif_make_list_cell(env, term, curr_head);
	}
	enif_make_reverse_list(env, curr_head, value);
    } else {
	if ((*ib_index + len) > in_buf_len)
	    return ASN1_LEN_ERROR;
	tmp_out_buff = enif_make_new_binary(env, len, value);
	memcpy(tmp_out_buff, in_buf + *ib_index, len);
	*ib_index = *ib_index + len;
    }
    return ASN1_OK;
}

struct ber_encode_mem_chunk {
    mem_chunk_t *next;
    int length;
    char *top;
    char *curr;
};

static int ber_encode(ErlNifEnv *env, ERL_NIF_TERM term, mem_chunk_t **curr, unsigned int *count) {

    const ERL_NIF_TERM *tv;
    unsigned int form;
    int arity;

    if (!enif_get_tuple(env, term, &arity, &tv))
	return ASN1_ERROR;

    form = enif_is_list(env, tv[1]) ? ASN1_CONSTRUCTED : ASN1_PRIMITIVE;

    switch (form) {
    case ASN1_PRIMITIVE: {
	ErlNifBinary value;
	if (!enif_inspect_binary(env, tv[1], &value))
	    return ASN1_ERROR;

	if (ber_check_memory(curr, value.size))
	    return ASN1_ERROR;
	memcpy((*curr)->curr - value.size + 1, value.data, value.size);
	(*curr)->curr -= value.size;
	*count += value.size;

	if (ber_encode_length(value.size, curr, count))
	    return ASN1_ERROR;

	break;
    }
    case ASN1_CONSTRUCTED: {
	ERL_NIF_TERM head, tail;
	unsigned int tmp_cnt;

	if(!enif_make_reverse_list(env, tv[1], &head))
	    return ASN1_ERROR;

	if (!enif_get_list_cell(env, head, &head, &tail)) {
	    if (enif_is_empty_list(env, tv[1])) {
		*((*curr)->curr) = 0;
		(*curr)->curr -= 1;
		(*count)++;
		break;
	    } else
		return ASN1_ERROR;
	}

	do {
	    tmp_cnt = 0;
	    if (ber_encode(env, head, curr, &tmp_cnt)) {
		return ASN1_ERROR;
	    }
	    *count += tmp_cnt;
	} while (enif_get_list_cell(env, tail, &head, &tail));

	if (ber_check_memory(curr, *count)) {
	    return ASN1_ERROR;
	}

	if (ber_encode_length(*count, curr, count)) {
	    return ASN1_ERROR;
	}

	break;
    }
    }

    // We need atleast 5 bytes to encode the next tlv
    if (ber_check_memory(curr, 3))
	return ASN1_ERROR;

    if (ber_encode_tag(env, tv[0], form, curr, count))
	return ASN1_ERROR;

    return ASN1_OK;
}

static int ber_encode_tag(ErlNifEnv *env, ERL_NIF_TERM tag, unsigned int form,
	mem_chunk_t **curr, unsigned int *count) {
    unsigned int class_tag_no, head_tag;
    if (!enif_get_uint(env, tag, &class_tag_no))
	return ASN1_ERROR;

    head_tag = form | ((class_tag_no & 0x30000) >> 10);
    class_tag_no = class_tag_no & 0xFFFF;

    if (class_tag_no <= 30) {
	*(*curr)->curr = head_tag | class_tag_no;
	(*curr)->curr -= 1;
	(*count)++;
	return ASN1_OK;
    } else {
	*(*curr)->curr = class_tag_no & 127;
	class_tag_no = class_tag_no >> 7;
	(*curr)->curr -= 1;
	(*count)++;

	while (class_tag_no > 0) {
	    *(*curr)->curr = (class_tag_no & 127) | 0x80;
	    class_tag_no >>= 7;
	    (*curr)->curr -= 1;
	    (*count)++;
	}

	*(*curr)->curr = head_tag | 0x1F;
	(*curr)->curr -= 1;
	(*count)++;

	return ASN1_OK;
    }
}

static int ber_encode_length(size_t size, mem_chunk_t **curr, unsigned int *count) {
    if (size < 128) {
	if (ber_check_memory(curr, 1u))
	    return ASN1_ERROR;
	*(*curr)->curr = size;
	(*curr)->curr -= 1;
	(*count)++;
    } else {
	int chunks = 0;
	if (ber_check_memory(curr, 8))
	    return ASN1_ERROR;

	while (size > 0)
	{
	    *(*curr)->curr = size & 0xFF;
	    size >>= 8;
	    (*curr)->curr -= 1;
	    (*count)++;
	    chunks++;
	}

	*(*curr)->curr = chunks | 0x80;
	(*curr)->curr -= 1;
	(*count)++;
    }
    return ASN1_OK;
}

static mem_chunk_t *ber_new_chunk(unsigned int length) {
    mem_chunk_t *new = enif_alloc(sizeof(mem_chunk_t));
    if (new == NULL)
	return NULL;
    new->next = NULL;
    new->top = enif_alloc(sizeof(char) * length);
    if (new->top == NULL) {
	free(new);
	return NULL;
    }
    new->curr = new->top + length - 1;
    new->length = length;
    return new;
}

static void ber_free_chunks(mem_chunk_t *chunk) {
    mem_chunk_t *curr, *next = chunk;
    while (next != NULL) {
	curr = next;
	next = curr->next;
	enif_free(curr->top);
	enif_free(curr);
    }
}

static int ber_check_memory(mem_chunk_t **curr, unsigned int needed) {
    mem_chunk_t *new;
    if ((*curr)->curr-needed >= (*curr)->top)
	return ASN1_OK;

    if ((new = ber_new_chunk((*curr)->length > needed ? (*curr)->length * 2 : (*curr)->length + needed)) == NULL)
	return ASN1_ERROR;
    new->next = *curr;
    *curr = new;
    return ASN1_OK;
}

static ERL_NIF_TERM encode_per_complete(ErlNifEnv* env, int argc,
	const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM err_code;
    ErlNifBinary in_binary;
    ErlNifBinary out_binary;
    int complete_len;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &in_binary))
	return enif_make_badarg(env);

    if (!enif_alloc_binary(in_binary.size, &out_binary))
	return enif_make_atom(env, "alloc_binary_failed");

    if (in_binary.size == 0)
	return enif_make_binary(env, &out_binary);

    if ((complete_len = per_complete(&out_binary, in_binary.data,
	    in_binary.size)) <= ASN1_ERROR) {
	enif_release_binary(&out_binary);
	if (complete_len == ASN1_ERROR
	)
	    err_code = enif_make_uint(env, '1');
	else
	    err_code = enif_make_uint(env, 0);
	return enif_make_tuple2(env, enif_make_atom(env, "error"), err_code);
    }
    if (complete_len < out_binary.size)
	enif_realloc_binary(&out_binary, complete_len);

    return enif_make_binary(env, &out_binary);
}

static ERL_NIF_TERM
make_ber_error_term(ErlNifEnv* env, unsigned int return_code,
		    unsigned int err_pos)
{
    ERL_NIF_TERM reason;
    ERL_NIF_TERM t;

    switch (return_code) {
    case ASN1_TAG_ERROR:
	reason = enif_make_atom(env, "invalid_tag");
	break;
    case ASN1_LEN_ERROR:
    case ASN1_INDEF_LEN_ERROR:
	reason = enif_make_atom(env, "invalid_length");
	break;
    case ASN1_VALUE_ERROR:
	reason = enif_make_atom(env, "invalid_value");
	break;
    default:
	reason = enif_make_atom(env, "unknown");
	break;
    }
    t = enif_make_tuple2(env, reason, enif_make_int(env, err_pos));
    return enif_make_tuple2(env, enif_make_atom(env, "error"), t);
}

static ERL_NIF_TERM decode_ber_tlv_raw(ErlNifEnv* env, int argc,
	const ERL_NIF_TERM argv[]) {
    ErlNifBinary in_binary;
    ERL_NIF_TERM return_term;
    unsigned int err_pos = 0, return_code;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &in_binary))
	return enif_make_badarg(env);

    return_code = ber_decode_begin(env, &return_term, in_binary.data,
				   in_binary.size, &err_pos);
    if (return_code != ASN1_OK) {
	return make_ber_error_term(env, return_code, err_pos);
    }
    return return_term;
}

static ERL_NIF_TERM encode_ber_tlv(ErlNifEnv* env, int argc,
	const ERL_NIF_TERM argv[]) {
    ErlNifBinary out_binary;
    unsigned int length = 0, pos = 0;
    int encode_err;
    mem_chunk_t *curr, *top;
    ERL_NIF_TERM err_code;

    curr = ber_new_chunk(40);

    if ((encode_err = ber_encode(env, argv[0], &curr, &length))
	    <= ASN1_ERROR) {
	ber_free_chunks(curr);
	err_code = enif_make_int(env, encode_err);
	return enif_make_tuple2(env, enif_make_atom(env, "error"), err_code);
    }

    if (!enif_alloc_binary(length, &out_binary)) {
	ber_free_chunks(curr);
	return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env,"oom"));
    }

    top = curr;

    while (curr != NULL) {
	length = curr->length - (curr->curr-curr->top) -1;
	if (length > 0)
	    memcpy(out_binary.data + pos, curr->curr+1, length);
	pos += length;
	curr = curr->next;
    }

    ber_free_chunks(top);

    return enif_make_binary(env, &out_binary);
}

static int is_ok_load_info(ErlNifEnv* env, ERL_NIF_TERM load_info) {
    int i;
    return enif_get_int(env, load_info, &i) && i == 1;
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    if (!is_ok_load_info(env, load_info))
	return -1;
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
	ERL_NIF_TERM load_info) {
    if (!is_ok_load_info(env, load_info))
	return -1;
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data) {

}


static ErlNifFunc nif_funcs[] =  {
    { "encode_per_complete", 1, encode_per_complete },
    { "decode_ber_tlv_raw", 1, decode_ber_tlv_raw },
    { "encode_ber_tlv", 1, encode_ber_tlv },
};

ERL_NIF_INIT(asn1rt_nif, nif_funcs, load, NULL, upgrade, unload)
