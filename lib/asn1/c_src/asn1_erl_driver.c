/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2010. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 *
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "erl_driver.h"
#include "ei.h"


/* #define ASN1_DEBUG 1 */

#define ASN1_OK 0
#define ASN1_ERROR -1
#define ASN1_COMPL_ERROR 1
#define ASN1_MEMORY_ERROR 0
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

#define ASN1_COMPLETE 1
#define ASN1_BER_TLV_DECODE 2
#define ASN1_BER_TLV_PARTIAL_DECODE 3

#define ASN1_NOVALUE 0

#define ASN1_SKIPPED 0
#define ASN1_OPTIONAL 1
#define ASN1_CHOOSEN 2


#define CEIL(X,Y) ((X-1) / Y + 1)

#define INVMASK(X,M) (X & (M ^ 0xff))
#define MASK(X,M) (X & M)

typedef struct {
  ErlDrvPort port;
  int buffer_size;
} asn1_data;

/* int min_alloc_bytes; */


static ErlDrvData asn1_drv_start(ErlDrvPort, char *);

static void asn1_drv_stop(ErlDrvData);

int asn1_drv_control(ErlDrvData, unsigned int, char *, int, char **, int);

int complete(ErlDrvBinary **,unsigned char *,unsigned char *, int);

int insert_octets(int, unsigned char **, unsigned char **, int *);

int insert_octets_except_unused(int, unsigned char **, unsigned char **,
				int *, int);

int insert_octets_as_bits_exact_len(int, int, unsigned char **,
				    unsigned char **, int *);

int insert_octets_as_bits(int, unsigned char **, unsigned char **,int *);

int pad_bits(int, unsigned char **, int *);

int insert_least_sign_bits(int, unsigned char, unsigned char **, int *);

int insert_most_sign_bits(int, unsigned char, unsigned char **, int *);

int insert_bits_as_bits(int, int, unsigned char **, unsigned char **, int *);

int insert_octets_unaligned(int, unsigned char **, unsigned char **, int);

int realloc_memory(ErlDrvBinary **,int,unsigned char **,unsigned char **);

int decode_begin(ErlDrvBinary **,unsigned char *, int, unsigned int *);

int decode(ErlDrvBinary **,int *,unsigned char *,int *, int);

int decode_tag(char *,int *,unsigned char *,int,int *);

int decode_value(int *,unsigned char *,int *,ErlDrvBinary **,int ,int);


/* declaration of functions used for partial decode of a BER encoded
   message */

int decode_partial(ErlDrvBinary **,unsigned char *, int);

int skip_tag(unsigned char *,int *,int);

int skip_length_and_value(unsigned char *,int *,int);

int get_tag(unsigned char *,int *,int);

int get_value(char *,unsigned char *,int *,int);

static ErlDrvEntry asn1_drv_entry = {
  NULL,              /* init, always NULL for dynamic drivers */
  asn1_drv_start,    /* start, called when port is opened */
  asn1_drv_stop,     /* stop, called when port is closed */
  NULL,              /* output, called when erlang has sent */
  NULL,              /* ready_input, called when input descriptor ready */
  NULL,              /* ready_output, called when output descriptor ready */
  "asn1_erl_drv",    /* char *driver_name, the argument to open_port */
  NULL,              /* finish, called when unloaded */
  NULL,              /* void * that is not used (BC) */
  asn1_drv_control,  /* control, port_control callback */
  NULL,              /* timeout, called on timeouts */
  NULL,              /* outputv, vector output interface */
  
  NULL,              /* ready_async */
  NULL,              /* flush */
  NULL,              /* call */
  NULL,              /* event */
  ERL_DRV_EXTENDED_MARKER,
  ERL_DRV_EXTENDED_MAJOR_VERSION,
  ERL_DRV_EXTENDED_MINOR_VERSION,
  ERL_DRV_FLAG_USE_PORT_LOCKING,
  NULL,              /* handle2 */
  NULL               /* process_exit */
};    



DRIVER_INIT(asn1_erl_drv) /* must match name in driver_entry */
{
  return &asn1_drv_entry;
}

static ErlDrvData asn1_drv_start(ErlDrvPort port, char *buff)
{
  /* extern int min_alloc_bytes; */
  char  *ptr;
  asn1_data* d;

  d = (asn1_data*)driver_alloc(sizeof(asn1_data));
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
  d->port = port;
  
  if ((ptr = getenv("ASN1_MIN_BUF_SIZE")) == NULL)
    d->buffer_size = 1024;
  else
    d->buffer_size = atoi(ptr);
  return (ErlDrvData)d;
}


static void asn1_drv_stop(ErlDrvData handle)
{
  driver_free((char*)handle);
}



int asn1_drv_control(ErlDrvData   handle, 
		     unsigned int command, 
		     char        *buf,
		     int          buf_len,
		     char       **res_buf,
		     int          res_buf_len) 
{
  char *complete_buf;
  int complete_len, decode_len;
  ErlDrvBinary *drv_binary;
  ErlDrvBinary **drv_bin_ptr;
  asn1_data* a_data;
  int min_alloc_bytes;
  unsigned int err_pos = 0; /* in case of error, return last correct position */
  int ret_err; /* return value in case of error in TLV decode, i.e. length of list in res_buf */

  /* In case previous call to asn1_drv_control resulted in a change of
     return value from binary to integer list */
  a_data = (asn1_data *)handle;
  min_alloc_bytes = a_data->buffer_size; 
  set_port_control_flags(a_data->port, PORT_CONTROL_FLAG_BINARY);

  if (command == ASN1_COMPLETE)
    { 
      if (buf_len==0) {
	  return 0; /* Avoid binary buffer overwrite (OTP-8451) */
      }
      /* Do the PER complete encode step */
      if ((drv_binary = driver_alloc_binary(buf_len))==NULL) {
	/* error handling */
	set_port_control_flags(a_data->port, 0);
	return ASN1_MEMORY_ERROR;
      }
      complete_buf = drv_binary->orig_bytes;
      if ((complete_len = complete(&drv_binary,complete_buf,buf,buf_len)) == ASN1_ERROR)
	{
	  /* error handling due to failure in complete */
	  /*       printf("error when running complete\n\r"); */
	  driver_free_binary(drv_binary);
	  set_port_control_flags(a_data->port, 0);
	  **res_buf = '1';
	  return ASN1_COMPL_ERROR;
	}
      /* printf("complete_len=%dbuf_len=%d,orig_size=%d\n\r",complete_len,buf_len,drv_binary->orig_size); */
      /* now the message is complete packed, return to Erlang */
      /*      if (complete_len < buf_len) {*/
      if (complete_len < drv_binary->orig_size) {	
	ErlDrvBinary *tmp;
	if ((tmp=driver_realloc_binary(drv_binary,complete_len)) == NULL){
	  /*error handling due to memory allocation failure */
	  driver_free_binary(drv_binary);
	  set_port_control_flags(a_data->port, 0);
	  return ASN1_MEMORY_ERROR;
	}else
	  drv_binary=tmp;
      }
      *res_buf = (char *)drv_binary;
      return complete_len;
    } else if (command == ASN1_BER_TLV_DECODE) { /* control == 2 */
      /* Do the tlv decode,
	 return the resulting term encoded on the Erlang 
	 external format */
/*       printf("driver: buffer_len = %d, min_alloc_bytes = %d\r\n",buf_len,min_alloc_bytes); */
      if ((drv_binary = driver_alloc_binary((buf_len*5)+min_alloc_bytes))==NULL) {
	/* error handling */
	set_port_control_flags(a_data->port, 0);
	return ASN1_MEMORY_ERROR;
      }
      drv_bin_ptr = &drv_binary;
      if ((decode_len = decode_begin(drv_bin_ptr,buf,buf_len,&err_pos)) <= ASN1_ERROR)
	{
	  /* error handling due to failure in decode  */
 	  char tmp_res_buf[5];
	  driver_free_binary(*drv_bin_ptr);
	  set_port_control_flags(a_data->port, 0);
	  
	  if(decode_len==ASN1_ERROR)
 	    tmp_res_buf[0]='1';
	  else if(decode_len==ASN1_TAG_ERROR)
 	    tmp_res_buf[0]='2';
	  else if(decode_len==ASN1_LEN_ERROR)
 	    tmp_res_buf[0]='3';
	  else if(decode_len==ASN1_INDEF_LEN_ERROR)
 	    tmp_res_buf[0]='4';
	  else if(decode_len==ASN1_VALUE_ERROR)
	    tmp_res_buf[0]='5';
/* 	  printf("err_pos=%d\r\n",err_pos); */
/* 	  printf("decode_len:%d\r\n",decode_len); */
	  ret_err = 1;
	  while(err_pos>0){
	    tmp_res_buf[ret_err] =(char)err_pos;/* c;*/
	    err_pos = err_pos >> 8;
	    ret_err++;
	  }
 	  strncpy(*res_buf,tmp_res_buf,ret_err);
	  return ret_err;
	}
/*       printf("decode_len=%d\r\n",decode_len); */
      if (decode_len < ((buf_len * 5) + min_alloc_bytes)) {
	/* not all memory was used => we have to reallocate */
	ErlDrvBinary *tmp;
	if ((tmp=driver_realloc_binary(*drv_bin_ptr,decode_len)) == NULL){
	  /*error handling due to memory allocation failure */
	  driver_free_binary(*drv_bin_ptr);
	  set_port_control_flags(a_data->port, 0);
	  return ASN1_MEMORY_ERROR;
	}else
	  *drv_bin_ptr=tmp;
      }
      *res_buf = (char *)(*drv_bin_ptr);
      return decode_len;
    } else { /*command == ASN1_BER_TLV_PARTIAL_DECODE */
      if ((drv_binary = driver_alloc_binary(buf_len))==NULL) {
	/* error handling */
	set_port_control_flags(a_data->port, 0);
	return ASN1_MEMORY_ERROR;
      }
      drv_bin_ptr = &drv_binary;
      if ((decode_len = decode_partial(drv_bin_ptr,buf,buf_len))
	  <= ASN1_ERROR) {
	/* error handling due to failure in decode  */
	driver_free_binary(*drv_bin_ptr);
	set_port_control_flags(a_data->port, 0);
	
/* 	printf("asn1_drv_control 1: decode_len=%d\r\n",decode_len); */
	  
	if(decode_len==ASN1_ERROR)
	  **res_buf = '1';
	return ASN1_DECODE_ERROR;
      }
      if (decode_len < buf_len) { 
	/* not all memory was used => we have to reallocate */
	ErlDrvBinary *tmp;
/* 	printf("asn1_drv_control 2: decode_len=%d\r\n",decode_len); */
	if ((tmp=driver_realloc_binary(*drv_bin_ptr,decode_len)) == NULL){
	  /*error handling due to memory allocation failure */
	  driver_free_binary(*drv_bin_ptr);
	  set_port_control_flags(a_data->port, 0);
	  return ASN1_MEMORY_ERROR;
	}else
	  *drv_bin_ptr=tmp;
      }
      *res_buf = (char *)(*drv_bin_ptr);
      return decode_len;
    }
}



/*
 * 
 * This section defines functionality for the complete encode of a
 * PER encoded message
 *
 */

int complete(ErlDrvBinary **drv_binary,unsigned char *complete_buf,
	     unsigned char *in_buf, int in_buf_len)
{
  int counter = in_buf_len;
  /* counter keeps track of number of bytes left in the
     input buffer */

  int buf_space = in_buf_len;
  /* This is the amount of allocated space left of the complete_buf. It
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
  int unused =  8;
  /* unused = [1,...,8] indicates how many of the rigthmost bits of 
     the byte that ptr points at that are unassigned */

  int no_bits,no_bytes,in_unused,desired_len,ret, saved_mem, needed, pad_bits;

  unsigned char val;
  
  in_ptr = in_buf;
  ptr = complete_buf;
  *ptr = 0x00;
  while(counter > 0) {
    counter--;
/*     printf("*in_ptr = %d\n\r",*in_ptr); */
    switch (*in_ptr) {
    case 0: 
      /* just one zero-bit should be added to the buffer */
      if(unused == 1){
	unused = 8;
	*++ptr = 0x00;
	buf_space--;
      } else 
	unused--;
      break;

    case 1:
      /* one one-bit should be added to the buffer */
      if(unused == 1){
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
      no_bits =  (int)*(++in_ptr);
      val = *(++in_ptr);
      counter -= 2;
      if ((ret=insert_least_sign_bits(no_bits,val,&ptr,&unused)) == ASN1_ERROR)
	return ASN1_ERROR;
      buf_space -= ret;
      break;

    case 20:
      /* in this case the next value in_ptr points at holds the number
	 of following bytes that holds the value that will be inserted
	 in the completed buffer */
      no_bytes = (int)*(++in_ptr);
      counter -= (no_bytes + 1);
      if ((counter<0) || 
	  (ret=insert_octets(no_bytes,&in_ptr,&ptr,&unused)) == ASN1_ERROR)
	return ASN1_ERROR;
      buf_space -= ret;
      break;

    case 21:
      /* in this case the next two bytes in_ptr points at holds the number
	 of following bytes that holds the value that will be inserted
	 in the completed buffer */
      no_bytes = (int)*(++in_ptr);
      no_bytes = no_bytes << 8;
      no_bytes = no_bytes | (int)*(++in_ptr);
      counter -= (2 + no_bytes);
      if ((counter<0) || 
	  (ret=insert_octets(no_bytes,&in_ptr,&ptr,&unused)) == ASN1_ERROR)
	return ASN1_ERROR;
      buf_space -= ret;
      break;

    case 30:
      /* If we call the following bytes, in the buffer in_ptr points at,
	 By1,By2,Rest then Rest is the value that will be transfered to
	 the completed buffer. By1 tells how many of the rightmost bits in 
	 Rest that should not be used. By2 is the length of Rest in bytes.*/
      in_unused = (int)*(++in_ptr);
      no_bytes = (int)*(++in_ptr);
      counter -= (2 + no_bytes);
/*        printf("%d: case 30: in_unused=%d, no_bytes=%d,counter=%d\n\r",__LINE__,in_unused,no_bytes,counter); */
      ret = -4711;
      if ((counter<0) || 
	  (ret=insert_octets_except_unused(no_bytes,&in_ptr,&ptr,&unused,in_unused)) == ASN1_ERROR)
	return ASN1_ERROR;
/*        printf("%d: ret=%d\n\r",__LINE__, ret); */
      buf_space -= ret;
      break;

    case 31:
      /* If we call the following bytes, in the buffer in_ptr points at,
	 By1,By2,By3,Rest then Rest is the value that will be transfered to
	 the completed buffer. By1 tells how many of the rightmost bits in 
	 Rest that should not be used. By2 and By3 is the length of 
	 Rest in bytes.*/
      in_unused = (int)*(++in_ptr);
      no_bytes = (int)*(++in_ptr);
      no_bytes = no_bytes << 8;
      no_bytes = no_bytes | (int)*(++in_ptr);
      counter -= (3 + no_bytes);
      if ((counter<0) || 
	  (ret=insert_octets_except_unused(no_bytes,&in_ptr,&ptr,&unused,in_unused)) == ASN1_ERROR)
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
      desired_len = (int)*(++in_ptr);
      no_bytes=(int)*(++in_ptr);
 
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
	if (realloc_memory(drv_binary,buf_size,&ptr,
			   &complete_buf) == ASN1_ERROR)
	  return ASN1_ERROR;
      }

      counter -= (2 + no_bytes);
     if ((counter<0) || 
	  (ret=insert_octets_as_bits_exact_len(desired_len,no_bytes,&in_ptr,
					       &ptr,&unused)) == ASN1_ERROR)
	return ASN1_ERROR;
      buf_space -= ret;
      break;
      
    case 41:
      /* Same as case 40 apart from By2, the length of Bin, which is in 
	 two bytes*/
      desired_len = (int)*(++in_ptr);
      no_bytes=(int)*(++in_ptr);
      no_bytes = no_bytes << 8;
      no_bytes = no_bytes | (int)*(++in_ptr);

      saved_mem = buf_space - counter;
      needed = CEIL((desired_len-unused),8) - no_bytes;
      if (saved_mem < needed) {
	/* Have to allocate more memory */
	buf_size += needed;
	buf_space += needed;
	if (realloc_memory(drv_binary,buf_size,&ptr,
			   &complete_buf) == ASN1_ERROR)
	  return ASN1_ERROR;
      }

      counter -= (3 + no_bytes);
      if ((counter<0) || 
	  (ret=insert_octets_as_bits_exact_len(desired_len,no_bytes,&in_ptr,
					       &ptr,&unused)) == ASN1_ERROR)
	return ASN1_ERROR;
      buf_space -= ret;
      break;
      
    case 42:
      /* Same as case 40 apart from By1, the desired length, which is in 
	 two bytes*/
      desired_len = (int)*(++in_ptr);
      desired_len = desired_len << 8;
      desired_len = desired_len | (int)*(++in_ptr);
      no_bytes=(int)*(++in_ptr);

      saved_mem = buf_space - counter;
      needed = CEIL((desired_len-unused),8) - no_bytes;
      if (saved_mem < needed) {
	/* Have to allocate more memory */
	buf_size += needed;
	buf_space += needed;
	if (realloc_memory(drv_binary,buf_size,&ptr,
			   &complete_buf) == ASN1_ERROR)
	  return ASN1_ERROR;
      }

      counter -= (3 + no_bytes);
      if ((counter<0) || 
	  (ret=insert_octets_as_bits_exact_len(desired_len,no_bytes,&in_ptr,
					       &ptr,&unused)) == ASN1_ERROR)
	return ASN1_ERROR;
      buf_space -= ret;
      break;
      
    case 43:
      /* Same as case 40 apart from By1 and By2, the desired length and
	 the length of Bin, which are in two bytes each. */
      desired_len = (int)*(++in_ptr);
      desired_len = desired_len << 8;
      desired_len = desired_len | (int)*(++in_ptr);
      no_bytes=(int)*(++in_ptr);
      no_bytes = no_bytes << 8;
      no_bytes = no_bytes | (int)*(++in_ptr);

      saved_mem = buf_space - counter;
      needed = CEIL((desired_len-unused),8) - no_bytes;
      if (saved_mem < needed) {
	/* Have to allocate more memory */
	buf_size += needed;
	buf_space += needed;
	if (realloc_memory(drv_binary,buf_size,&ptr,
			   &complete_buf) == ASN1_ERROR)
	  return ASN1_ERROR;
      }

      counter -= (4 + no_bytes);
      if ((counter<0) || 
	  (ret=insert_octets_as_bits_exact_len(desired_len,no_bytes,&in_ptr,
					       &ptr,&unused)) == ASN1_ERROR)
	return ASN1_ERROR;
      buf_space -= ret;
      break;
      
    case 45:
      /* This case assumes that the following bytes in the incoming buffer
	 (called By1,By2,Bin) is By1, which is the number of bits (n) that 
	 will be inserted in the completed buffer. By2 is the number of
	 bytes in Bin. Each bit in the buffer Bin should be inserted from
	 the leftmost until the nth.*/
      desired_len = (int)*(++in_ptr);
      no_bytes=(int)*(++in_ptr);
 
      saved_mem = buf_space - counter;
      needed = CEIL((desired_len-unused),8) - no_bytes;
/*       printf("buf_space=%d, counter=%d, needed=%d",buf_space,counter,needed);  */
      if (saved_mem < needed) {
	/* Have to allocate more memory */
	buf_size += needed;
	buf_space += needed;
	if (realloc_memory(drv_binary,buf_size,&ptr,
			   &complete_buf) == ASN1_ERROR)
	  return ASN1_ERROR;
      }

      counter -= (2 + no_bytes);
/*       printf("calling insert_bits_as_bits: desired_len=%d, no_bytes=%d\n\r",desired_len,no_bytes); */
/*       printf("1in_ptr=%d\n\r",in_ptr); */

      if((counter<0) || 
	 (ret=insert_bits_as_bits(desired_len,no_bytes,&in_ptr,
				  &ptr,&unused)) == ASN1_ERROR)
	return ASN1_ERROR;
/*       printf("2in_ptr=%d, ptr=%d, complete_buf=%d\n\r",in_ptr,ptr,complete_buf); */
/*       printf("buf_space=%d, ret=%d, counter=%d\n\r",buf_space,ret,counter); */
      buf_space -= ret;
      break;
      
    case 46:
      /* Same as case 45 apart from By1, the desired length, which is
	 in two bytes. */
      desired_len = (int)*(++in_ptr);
      desired_len = desired_len << 8;
      desired_len = desired_len | (int)*(++in_ptr);
      no_bytes=(int)*(++in_ptr);
 
      saved_mem = buf_space - counter;
      needed = CEIL((desired_len-unused),8) - no_bytes;
      if (saved_mem < needed) {
	/* Have to allocate more memory */
	buf_size += needed;
	buf_space += needed;
	if (realloc_memory(drv_binary,buf_size,&ptr,
			   &complete_buf) == ASN1_ERROR)
	  return ASN1_ERROR;
      }

      counter -= (3 + no_bytes);
      if((counter<0) || 
	 (ret=insert_bits_as_bits(desired_len,no_bytes,&in_ptr,
				  &ptr,&unused)) == ASN1_ERROR)
	return ASN1_ERROR;
      buf_space -= ret;
      break;
      
    case 47:
      /* Same as case 45 apart from By1 and By2, the desired length
	 and the length of Bin, which are in two bytes each. */
      desired_len = (int)*(++in_ptr);
      desired_len = desired_len << 8;
      desired_len = desired_len | (int)*(++in_ptr);
      no_bytes=(int)*(++in_ptr);
      no_bytes = no_bytes << 8;
      no_bytes = no_bytes | (int)*(++in_ptr);
 
      saved_mem = buf_space - counter;
      needed = CEIL((desired_len-unused),8) - no_bytes;
      if (saved_mem < needed) {
	/* Have to allocate more memory */
	buf_size += needed;
	buf_space += needed;
	if (realloc_memory(drv_binary,buf_size,&ptr,
			   &complete_buf) == ASN1_ERROR)
	  return ASN1_ERROR;
      }

      counter -= (4 + no_bytes);
      if((counter<0) || 
	 (ret=insert_bits_as_bits(desired_len,no_bytes,&in_ptr,
				  &ptr,&unused)) == ASN1_ERROR)
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
  if ((unused == 8) && (ptr != complete_buf)) 
    return (ptr - complete_buf);
  else {
    ptr++; /* octet align buffer */
    return (ptr - complete_buf);
  }
}


int realloc_memory(ErlDrvBinary **drv_binary,
		   int amount,
		   unsigned char **ptr,
		   unsigned char **complete_buf) {

  ErlDrvBinary *tmp_bin;
  int i;

/*   printf("realloc_momory: amount = %d\n",amount); */
  if ((tmp_bin=driver_realloc_binary(*drv_binary,amount)) == NULL) {
    /*error handling due to memory allocation failure */
/*     printf("error when allocating memory\n"); */
    return ASN1_ERROR;
  }else {
    i = *ptr - *complete_buf;
    *drv_binary=tmp_bin;
    *complete_buf = (*drv_binary)->orig_bytes;
    *ptr = *complete_buf + i;
  }
  return ASN1_OK;
}


int insert_most_sign_bits(int no_bits,
			  unsigned char val,
			  unsigned char **output_ptr,
			  int *unused) {
  unsigned char *ptr = *output_ptr;
  
  if (no_bits < *unused){
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


int insert_least_sign_bits(int no_bits,
			   unsigned char val,
			   unsigned char **output_ptr,
			   int *unused) {
  unsigned char *ptr = *output_ptr;
  int ret = 0;

  if (no_bits < *unused){
    *ptr = *ptr | (val << (*unused - no_bits));
    *unused -= no_bits;
  } else if (no_bits == *unused){
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

/* pad_bits adds no_bits bits in the buffer that output_ptr 
   points at.
 */
int pad_bits(int no_bits, unsigned char **output_ptr, int *unused) 
  {
    unsigned char *ptr = *output_ptr;
    int ret = 0;
    
    while (no_bits > 0) {
      if(*unused == 1){
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
int insert_bits_as_bits(int desired_no,
			int no_bytes,
			unsigned char **input_ptr,
			unsigned char **output_ptr,
			int *unused)
{
  unsigned char *in_ptr = *input_ptr;
  unsigned char val;
  int no_bits, ret, ret2;

  if (desired_no == (no_bytes * 8)) {
    if(insert_octets_unaligned(no_bytes,&in_ptr,output_ptr,*unused)
       == ASN1_ERROR)
      return ASN1_ERROR;
    ret = no_bytes;
  }
  else if (desired_no < (no_bytes * 8)) {
/*     printf("insert_bits_as_bits 1\n\r"); */
    if(insert_octets_unaligned(desired_no/8,&in_ptr,output_ptr,*unused)
       == ASN1_ERROR)
      return ASN1_ERROR;
/*     printf("insert_bits_as_bits 2\n\r"); */
    val = *++in_ptr;
/*     printf("val = %d\n\r",(int)val); */
    no_bits = desired_no % 8;
/*     printf("no_bits = %d\n\r",no_bits); */
    insert_most_sign_bits(no_bits,val,output_ptr,unused);
    ret = CEIL(desired_no,8);
  }
  else {
    if(insert_octets_unaligned(no_bytes,&in_ptr,output_ptr,*unused) 
       == ASN1_ERROR)
      return ASN1_ERROR;
    ret2 = pad_bits(desired_no - (no_bytes * 8),output_ptr,unused);
/*     printf("ret2 = %d\n\r",ret2); */
    ret = CEIL(desired_no,8);
/*     printf("ret = %d\n\r",ret); */
  }
/*   printf("*unused = %d\n\r",*unused); */
  *input_ptr = in_ptr;
  return ret;
}


/* insert_octets_as_bits_exact_len */
int
insert_octets_as_bits_exact_len(int desired_len,
				int in_buff_len,
				unsigned char **in_ptr,
				unsigned char **ptr,
				int *unused)
{
  int ret = 0;
  int ret2 = 0;

  if (desired_len == in_buff_len) {
    if ((ret = insert_octets_as_bits(in_buff_len,in_ptr,ptr,unused)) == ASN1_ERROR)
      return ASN1_ERROR;
  }
  else if(desired_len > in_buff_len) {
    if((ret = insert_octets_as_bits(in_buff_len,in_ptr,ptr,unused)) == ASN1_ERROR)
      return ASN1_ERROR;
    /* now pad with zero bits */
/*     printf("~npad_bits: called with %d bits padding~n~n~r",desired_len - in_buff_len); */
    if ((ret2=pad_bits(desired_len - in_buff_len,ptr,unused)) == ASN1_ERROR)
      return ASN1_ERROR;
  }
  else {/* desired_len < no_bits */
    if ((ret=insert_octets_as_bits(desired_len,in_ptr,ptr,unused)) == ASN1_ERROR)
      return ASN1_ERROR;
    /* now remove no_bits - desired_len bytes from in buffer */
    *in_ptr += (in_buff_len - desired_len);
  }
  return (ret+ret2);
}



/* insert_octets_as_bits takes no_bytes bytes from the buffer that input_ptr
   points at and inserts the least significant bit of it in the buffer that
   output_ptr points at. Each byte in the input buffer must be 1 or 0
   otherwise the function returns ASN1_ERROR. The output buffer is concatenated
   without alignment.
 */
int insert_octets_as_bits(int no_bytes,
			  unsigned char **input_ptr,
			  unsigned char **output_ptr,
			  int *unused)
{
  unsigned char *in_ptr = *input_ptr;
  unsigned char *ptr = *output_ptr;
  int used_bits = 8 - *unused;

  while (no_bytes > 0) {
    switch (*++in_ptr) {
    case 0:
      if(*unused == 1){
	*unused = 8;
	*++ptr = 0x00;
      } else 
	(*unused)--;
      break;
    case 1:
      if(*unused == 1){
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
  return ((used_bits+no_bytes) / 8); /*return number of new bytes
				      in completed buffer */
}

/* insert_octets inserts bytes from the input buffer, *input_ptr,
   into the output buffer, *output_ptr. Before the first byte is
   inserted the input buffer is aligned.
 */
int insert_octets(int no_bytes,
		  unsigned char **input_ptr,
		  unsigned char **output_ptr,
		  int *unused)
{
    unsigned char *in_ptr = *input_ptr;
    unsigned char *ptr = *output_ptr;
    int ret = 0;
      
    if (*unused != 8) {/* must align before octets are added */
      *++ptr = 0x00;
      ret++;
      *unused = 8;
    }
    while(no_bytes > 0) {
      *ptr = *(++in_ptr);
      *++ptr = 0x00;
      /*      *unused = *unused - 1; */
      no_bytes--;
    }
  *input_ptr = in_ptr;
  *output_ptr = ptr;
  return (ret + no_bytes);
}

/* insert_octets_unaligned inserts bytes from the input buffer, *input_ptr,
   into the output buffer, *output_ptr.No alignment is done.
 */
int insert_octets_unaligned(int no_bytes,
			    unsigned char **input_ptr,
			    unsigned char **output_ptr,
			    int unused)
{
  unsigned char *in_ptr = *input_ptr;
  unsigned char *ptr = *output_ptr;
  int n = no_bytes;
  unsigned char val;
  
  while (n > 0) {
    if (unused == 8) {
      *ptr = *++in_ptr;
      *++ptr = 0x00;
    }else {
      val = *++in_ptr;
      *ptr =  *ptr | val >> (8 - unused);
      *++ptr = 0x00;
      *ptr = val << unused;
    }
    n--;
  }
  *input_ptr = in_ptr;
  *output_ptr = ptr;
  return no_bytes;
}


int insert_octets_except_unused(int no_bytes,
				unsigned char **input_ptr,
				unsigned char **output_ptr,
				int *unused,
				int in_unused)
{
  unsigned char *in_ptr = *input_ptr;
  unsigned char *ptr = *output_ptr;
  int val, no_bits;
  int ret = 0;
  
  if (in_unused == 0){
/*      printf("%d: insert_octets_except_unused: if\n\r",__LINE__); */
    if ((ret = insert_octets_unaligned(no_bytes,&in_ptr,&ptr,
				       *unused)) == ASN1_ERROR)
      return ASN1_ERROR;
  }
    else {
/*        printf("%d: insert_octets_except_unused: else\n\r",__LINE__); */
      if ((ret=insert_octets_unaligned(no_bytes - 1,&in_ptr,&ptr,*unused)) != ASN1_ERROR) {
	val = (int) *(++in_ptr);
	no_bits = 8 - in_unused;
	/* no_bits is always less than *unused since the buffer is
	   octet aligned after insert:octets call, so the following
	   if clasuse is obsolete I think */
	if(no_bits < *unused){
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
/*    printf("%d: insert_octets_except_unused: ret=%d\n\r",__LINE__,ret); */
  return ret;
}



/*
 * 
 * This section defines functionality for the partial decode of a
 * BER encoded message
 *
 */

/*
 * int decode(ErlDrvBinary **drv_binary,unsigned char *decode_buf,
 *            unsigned char *in_buf, int in_buf_len) 
 * drv_binary is a pointer to a pointer to an allocated driver binary.
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
 * The function returns a value in Erlnag term format:
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
int decode_begin(ErlDrvBinary **drv_binary,unsigned char *in_buf, int in_buf_len, unsigned int *err_pos)
{
  int maybe_ret;
  char *decode_buf = (*drv_binary)->orig_bytes;
  int ei_index = 0;
  int ib_index = 0;
  /* ei_index is the index used by the ei functions to encode an
     Erlang term into the buffer decode_buf */
  /* ib_index is the index were to read the next byte from in_buf */


#ifdef ASN1_DEBUG
    printf("decode_begin1: ei_index=%d, ib_index=%d\n\r",ei_index,ib_index);
#endif
  /* the first byte must be a "version magic" */
  if(ei_encode_version(decode_buf,&ei_index) == ASN1_ERROR)
    return ASN1_ERROR;		/* 1 byte */
#ifdef ASN1_DEBUG
    printf("decode_begin2: ei_index=%d, ib_index=%d\n\r",ei_index,ib_index);
#endif
  if (ei_encode_tuple_header(decode_buf,&ei_index,2) == ASN1_ERROR)
    return ASN1_ERROR;		/* 2 bytes */
#ifdef ASN1_DEBUG
    printf("decode_begin3: ei_index=%d, ib_index=%d\n\r",ei_index,ib_index);
#endif
  if((maybe_ret=decode(drv_binary,&ei_index,in_buf,&ib_index,in_buf_len)) <= ASN1_ERROR) 
    {
      *err_pos = ib_index;
#ifdef ASN1_DEBUG
       printf("err_pos=%d,ib_index=%d\r\n",*err_pos,ib_index);
#endif
      return maybe_ret;
    };
  
  decode_buf = (*drv_binary)->orig_bytes; /* maybe a realloc during decode_value */
#ifdef ASN1_DEBUG
    printf("decode_begin4: in_buf_len=%d, ei_index=%d, ib_index=%d\n\r",
  	 in_buf_len,ei_index,ib_index);
#endif
  /* "{{TagNo,Value},Rest}" */
  if (ei_encode_binary(decode_buf,&ei_index,&(in_buf[ib_index]),in_buf_len-ib_index)
      == ASN1_ERROR)		/* at least 5 bytes */
    return ASN1_ERROR;
#ifdef ASN1_DEBUG
    printf("decode_begin5: ei_index=%d, ib_index=%d\n\r",ei_index,ib_index);
#endif
  return ei_index;
}

int decode(ErlDrvBinary **drv_binary,int *ei_index,unsigned char *in_buf,
	   int *ib_index, int in_buf_len)
{
  int maybe_ret;
  char *decode_buf = (*drv_binary)->orig_bytes;
  int form;
#ifdef ASN1_DEBUG
    printf("decode 1\n\r");
#endif
  if (((*drv_binary)->orig_size - *ei_index) < 19) {/* minimum amount of bytes */
    /* allocate more memory */
    if (realloc_decode_buf(drv_binary,(*drv_binary)->orig_size * 2) == 
	ASN1_ERROR)
      return ASN1_ERROR;
    decode_buf = (*drv_binary)->orig_bytes;
  }
/*    printf("decode 2\n\r"); */
    /* "{" */
    if (ei_encode_tuple_header(decode_buf,ei_index,2) == ASN1_ERROR)
    return ASN1_ERROR;		/* 2 bytes */
#ifdef ASN1_DEBUG
    printf("decode 3:orig_size=%d, ei_index=%d, ib_index=%d\n\r",(*drv_binary)->orig_size,*ei_index,*ib_index);
#endif

    /*buffer must hold at least two bytes*/	
    if ((*ib_index +2) > in_buf_len)
      return ASN1_VALUE_ERROR;
    /* "{{TagNo," */
    if ((form = decode_tag(decode_buf,ei_index,in_buf,in_buf_len,ib_index)) <= ASN1_ERROR)
      return form;		/* 5 bytes */
#ifdef ASN1_DEBUG
    printf("i_i=%d,in_buf_len=%d\r\n",*ei_index,in_buf_len);
#endif
    if (*ib_index >= in_buf_len){
      return ASN1_TAG_ERROR;
    }
#ifdef ASN1_DEBUG
    printf("decode 5 ib_index=%d\n\r",*ib_index);
#endif
    /* buffer must hold at least one byte (0 as length and nothing as
       value) */
    /* "{{TagNo,Value}," */
    if ((maybe_ret=decode_value(ei_index,in_buf,ib_index,drv_binary,form,
				in_buf_len)) <= ASN1_ERROR)
      return maybe_ret;		/* at least 5 bytes */
#ifdef ASN1_DEBUG
     printf("decode 7\n\r");
#endif
    return *ei_index;
}

/* 
 * decode_tag decodes the BER encoded tag in in_buf and puts it in the
 * decode_buf encoded by the Erlang extern format as an Erlang term.
 */
int decode_tag(char *decode_buf,int *db_index,unsigned char *in_buf,
	       int in_buf_len, int *ib_index)
{
  int tag_no, tmp_tag, form;
  

  /* first get the class of tag and bit shift left 16*/
  tag_no = ((MASK(in_buf[*ib_index],ASN1_CLASS)) << 10);

  form = (MASK(in_buf[*ib_index],ASN1_FORM));
#ifdef ASN1_DEBUG
     printf("decode_tag0:ii=%d, tag_no=%d, form=%d.\r\n",
	    *ib_index,tag_no,form);
#endif

  /* then get the tag number */
  if((tmp_tag = (int) INVMASK(in_buf[*ib_index],ASN1_CLASSFORM)) < 31) {
    ei_encode_ulong(decode_buf,db_index,tag_no+tmp_tag); /* usual case */
    (*ib_index)++;
#ifdef ASN1_DEBUG
     printf("decode_tag1:ii=%d.\r\n",*ib_index);
#endif
  }
  else
    {
      int n = 0; /* n is used to check that the 64K limit is not
		    exceeded*/
#ifdef ASN1_DEBUG
     printf("decode_tag1:ii=%d, in_buf_len=%d.\r\n",*ib_index,in_buf_len);
#endif

      /* should check that at least three bytes are left in
	 in-buffer,at least two tag byte and at least one length byte */
      if ((*ib_index +3) > in_buf_len)
	return ASN1_VALUE_ERROR;
      (*ib_index)++;
#ifdef ASN1_DEBUG
       printf("decode_tag2:ii=%d.\r\n",*ib_index);
#endif
      /* The tag is in the following bytes in in_buf as 
	 1ttttttt 1ttttttt ... 0ttttttt, where the t-bits
	 is the tag number*/
      /* In practice is the tag size limited to 64K, i.e. 16 bits. If
	 the tag is greater then 64K return an error */
      while (((tmp_tag = (int)in_buf[*ib_index]) >= 128) && n < 2){
	/* m.s.b. = 1 */
	tag_no = tag_no + (MASK(tmp_tag,ASN1_LONG_TAG) << 7);
	(*ib_index)++;
#ifdef ASN1_DEBUG
 	printf("decode_tag3:ii=%d.\r\n",*ib_index);
#endif
	n++;
      };
      if ((n==2) && in_buf[*ib_index] > 3)
	return ASN1_TAG_ERROR; /* tag number > 64K */
      tag_no = tag_no + in_buf[*ib_index];
      (*ib_index)++;
#ifdef ASN1_DEBUG
       printf("decode_tag4:ii=%d.\r\n",*ib_index);
#endif
      ei_encode_ulong(decode_buf,db_index,tag_no);
    }
  return form;
}


/*
 * decode_value decodes the BER encoded length and value fields in the
 * in_buf and puts the value part in the decode_buf as an Erlang term
 * encoded by the Erlang extern format
 */
int decode_value(int *ei_index,unsigned char *in_buf,
		 int *ib_index,ErlDrvBinary **drv_binary,int form,
		 int in_buf_len)
{
  int maybe_ret;
  char *decode_buf = (*drv_binary)->orig_bytes;
  int len, lenoflen;
  int indef = 0;
  
#ifdef ASN1_DEBUG
    printf("decode_value1:ib_index=%d\n\r",*ib_index);
#endif
  if (((in_buf[*ib_index]) & 0x80) == ASN1_SHORT_DEFINITE_LENGTH) {
    len = in_buf[*ib_index];
    if (len > (in_buf_len - (*ib_index + 1)))
      return ASN1_LEN_ERROR;
  }
  else if (in_buf[*ib_index] == ASN1_INDEFINITE_LENGTH)
    indef = 1;
  else /* long definite length */ {
    lenoflen = (in_buf[*ib_index] & 0x7f); /*length of length */
#ifdef ASN1_DEBUG
     printf("decode_value,lenoflen:%d\r\n",lenoflen);
#endif
    len = 0;
    while (lenoflen-- && (*ib_index <= in_buf_len)) {
      (*ib_index)++;
#ifdef ASN1_DEBUG
       printf("decode_value1:ii=%d.\r\n",*ib_index);
#endif
      len = (len << 8) + in_buf[*ib_index];
    }
    if (len > (in_buf_len - (*ib_index + 1)))
      return ASN1_LEN_ERROR;
  }
  (*ib_index)++;
#ifdef ASN1_DEBUG
   printf("decode_value2:ii=%d.\r\n",*ib_index);
#endif
  if (indef == 1)
    { /* in this case it is desireably to check that indefinite length
	 end bytes exist in inbuffer */
      while (!(in_buf[*ib_index]==0 && in_buf[*ib_index + 1]==0)) {
#ifdef ASN1_DEBUG
	printf("decode_value while:ib_index=%d in_buf_len=%d\n\r",
	 *ib_index,in_buf_len);
#endif
	if(*ib_index >= in_buf_len)
	  return ASN1_INDEF_LEN_ERROR;
	ei_encode_list_header(decode_buf,ei_index,1); /* 5 bytes */
	if((maybe_ret=decode(drv_binary,ei_index,in_buf,
			     ib_index,in_buf_len)) <= ASN1_ERROR)
	  return maybe_ret;
	decode_buf = (*drv_binary)->orig_bytes;
      }
      (*ib_index) += 2; /* skip the indefinite length end bytes */
#ifdef ASN1_DEBUG
       printf("decode_value3:ii=%d.\r\n",*ib_index);
#endif
      ei_encode_empty_list(decode_buf,ei_index); /* 1 byte */
    }
  else if (form == ASN1_CONSTRUCTED)
    {
      int end_index = *ib_index + len;
      if(end_index > in_buf_len)
	return ASN1_LEN_ERROR;
      while (*ib_index < end_index) {

#ifdef ASN1_DEBUG
  	printf("decode_value3:*ib_index=%d, end_index=%d\n\r",*ib_index,end_index);
#endif
	ei_encode_list_header(decode_buf,ei_index,1); /* 5 bytes */
	if((maybe_ret=decode(drv_binary,ei_index,in_buf,
			     ib_index,in_buf_len))<=ASN1_ERROR)
	  return maybe_ret;
	decode_buf = (*drv_binary)->orig_bytes;
      }
      ei_encode_empty_list(decode_buf,ei_index); /* 1 byte */
    }
  else
    {
      if (((*drv_binary)->orig_size - *ei_index) < 10+len) { /* 5+len for the  binary*/
	if (realloc_decode_buf(drv_binary,(*drv_binary)->orig_size * 2) == 
	    ASN1_ERROR)
	  return ASN1_ERROR;
	decode_buf = (*drv_binary)->orig_bytes;
      }
      if((*ib_index + len) > in_buf_len)
	return ASN1_LEN_ERROR;
      ei_encode_binary(decode_buf,ei_index,&in_buf[*ib_index],len);
      *ib_index = *ib_index + len;
#ifdef ASN1_DEBUG
       printf("decode_value4:ii=%d.\r\n",*ib_index);
#endif
    }
  return ASN1_OK;
}

int realloc_decode_buf(ErlDrvBinary **drv_binary,int amount) {
  ErlDrvBinary *tmp_bin;

  if ((tmp_bin=driver_realloc_binary(*drv_binary,amount)) == NULL)
    return ASN1_ERROR;
  *drv_binary = tmp_bin;
  return ASN1_OK;
}



/*
 * int decode_partial(drv_binary,in_buf,in_buf_len) 
 */
/*
 * The in_buf contains two parts: first information about which value
 * will be decoded, as a sequence of tags and tag codes, then the
 * encoded BER value. First of all comes a length field that tells how
 * many following bytes contains the sequence of tags. Then starts the
 * BER encoded message. The tag sequence length field is a single
 * byte. The sequence of tags/tag codes may be one of the codes
 * ASN1_SKIPPED, ASN1_CHOOSEN and a tag or ASN1_OPTIONAL and a
 * tag. ASN1_SKIPPED means that the following tag is mandatory and is
 * skipped. ASN1_CHOOSEN means that the value of this tag shall, if
 * this was the last tag in tag sequence, be returned or be searched
 * in for the next tag. ASN1_OPTIONAL means that this tag shall be
 * skipped but it may be missing. Each tag in the tag sequence
 * correspond to a tag in the BER encoded message. If the decode
 * arives to a position where there is no matching tag, an error is
 * returned (if it wasn't the last tag and it was OPTIONAL). After the
 * right value has been detected it is returned in the out_buf.
 *
 */
int decode_partial(ErlDrvBinary **drv_binary,unsigned char *in_buf, int in_buf_len)
{
  char *out_buf = (*drv_binary)->orig_bytes;
  int tag_index_val = 1;
  int msg_index_val;
  int *msg_index, *tag_index, tmp_index;
  int tag_seq_length;
  char tag_code; /* one of ASN1_SKIPPED, ASN1_OPTIONAL, ASN1_CHOOSEN */
  int wanted_tag, next_tag;
  int buf_end_index = in_buf_len;
  int ret = 0, length, old_index;
  
  tag_index = &tag_index_val;
  tag_seq_length = in_buf[0];
  msg_index = &msg_index_val;
  *msg_index = tag_seq_length + 1;


/*   printf("decode_partial 1: in_buf_len=%d, tag_index=%d, msg_index=%d\r\n,tag_seq_length=%d\r\n",in_buf_len,*tag_index,*msg_index,tag_seq_length); */
  while(*tag_index < tag_seq_length) {
    switch(in_buf[*tag_index]) {
    case ASN1_SKIPPED:
/*       printf("decode_partial ASN1_SKIPPED: in_buf[*msg_index]=%d\r\n",in_buf[*msg_index]); */
      (*tag_index)++;
/*       printf("decode_partial ASN1_SKIPPED 2: *msg_index=%d\r\n",*msg_index); */
      skip_tag(in_buf,msg_index,buf_end_index);
/*       printf("decode_partial ASN1_SKIPPED 3: *msg_index=%d\r\n",*msg_index); */
      skip_length_and_value(in_buf,msg_index,buf_end_index);
/*       printf("decode_partial ASN1_SKIPPED 4: *msg_index=%d\r\n",*msg_index); */
      break;
    case ASN1_OPTIONAL:
      (*tag_index)++;
/*       printf("decode_partial ASN1_OPTIONAL: in_buf[*tag_index]=%d\r\n",in_buf[*tag_index]); */
      wanted_tag = in_buf[*tag_index];
      (*tag_index)++;
      tmp_index = *msg_index;
      next_tag = get_tag(in_buf,msg_index,buf_end_index);
      if (wanted_tag != next_tag) {
	*msg_index = tmp_index;
      } else 
	skip_length_and_value(in_buf,msg_index,buf_end_index);
      break;
    case ASN1_CHOOSEN:
/*       printf("decode_partial ASN1_CHOOSEN: in_buf[*msg_index]=%d, *msg_index=%d\r\n",in_buf[*msg_index],*msg_index); */
      (*tag_index)++;
      wanted_tag = in_buf[*tag_index];
      (*tag_index)++;
      old_index = *msg_index;
/*       printf("decode_partial ASN1_CHOOSEN 2: *msg_index=%d\r\n",*msg_index); */
      next_tag = get_tag(in_buf,msg_index,buf_end_index);
/*       printf("decode_partial ASN1_CHOOSEN 3: *msg_index=%d\r\n,wanted_tag=%d, next_tag=%d\r\n",*msg_index,wanted_tag,next_tag); */
      if (wanted_tag != next_tag)
	return ASN1_NOVALUE; /* an empty binary will be returned to Erlang */
      if (*tag_index == (tag_seq_length + 1)) { 
	/* get the value and return*/
	if((ret = get_value(out_buf,in_buf,msg_index,buf_end_index)) <= ASN1_ERROR)
	  return ASN1_ERROR;
	return ret;
      } 
      else { 
	/* calculate the length of the sub buffer and let *msg_index
	   be at the value part of this BER encoded type*/
	int indef;
	indef = 0;
	length = get_length(in_buf,msg_index,&indef,buf_end_index);
/* 	printf("decode_partial ASN1_CHOOSEN 4: length=%d, *msg_index=%d\r\n",length,*msg_index); */
	if ((length == 0) && (indef == 1)) {
	  /* indefinite length of value */
	  old_index = *msg_index;
	  length = skip_length_and_value(in_buf,msg_index,buf_end_index);
	  *msg_index = old_index;
	  buf_end_index = *msg_index + length - 2; 
	  /* remove two bytes due to indefinete length end zeros */
	} else 
	  buf_end_index = (*msg_index + length);
      }
      break;
    default:
      return ASN1_ERROR;
    }
  }
  return ASN1_ERROR;
}


/*
 * int skip_tag(unsigned char *in_buf,int *index,int buf_len)
 * steps past the BER encoded tag in in_buf and updates *index.
 * Returns the number of skipped bytes.
 */
int skip_tag(unsigned char *in_buf,int *index,int buf_len)
{
  int start_index = *index;
  if ((MASK(in_buf[*index],ASN1_TAG)) == 31){
    do {
      (*index)++;
      if (*index >= buf_len)
	return ASN1_ERROR;
    }
    while(in_buf[*index] >=128);
  }
  (*index)++;
  return (*index - start_index);
}


/*
 * int skip_length_and_value(unsigned char *in_buf,int *index,int buf_len)
 * steps past the BER encoded length and value in in_buf and updates *index.
 * returns the length if the skipped "length value".
 * Returns the number of skipped bytes.
 */
int skip_length_and_value(unsigned char *in_buf,int *index,int buf_len)
{
  long len;
  int indef = 0, lenoflen;
  int start_index = *index;

  if ((MASK(in_buf[*index],0x80)) == ASN1_SHORT_DEFINITE_LENGTH){
    len = in_buf[*index];
    if (len > (buf_len - (*index + 1)))
      return ASN1_LEN_ERROR;
  } else if (in_buf[*index] == ASN1_INDEFINITE_LENGTH)
    indef = 1;
  else /* long definite length */ {
    lenoflen = (in_buf[*index] & 0x7f); /*length of length */
    len = 0;
    while (lenoflen--) {
      (*index)++;
      len = (len << 8) + in_buf[*index];
    }
    if (len > (buf_len - (*index + 1)))
      return ASN1_LEN_ERROR;
  }
  (*index)++;
  if (indef == 1)
    {
      while(!(in_buf[*index]==0 && in_buf[*index + 1]==0)) {
	skip_tag(in_buf,index,buf_len);
	skip_length_and_value(in_buf,index,buf_len);
      }
      (*index) += 2;
    }
  else 
    (*index) += len;
  return (*index - start_index);
}

/* int get_tag(unsigned char *in_buf,int *index)
 *
 * assumes next byte/bytes in in_buf is an encoded BER tag. A tag
 * number has theoretically no upper limit in size. Here the tag
 * number is assumed to be less than 64K. Returns an integer value
 * on the format:
 * xxxxxxxx xxxxxxcc tttttttt tttttttt
 * the x-bits are 0 (insignificant)
 * the c-bits are the class of the tag
 * the t-bits are the tag number. This implies that the tag number
 * is limited to 64K-1
 * 
 */
int get_tag(unsigned char *in_buf,int *index,int buf_len)
{
  int tag_no = 0,tmp_tag = 0;

  tag_no = (MASK(in_buf[*index],ASN1_CLASSFORM));
  if ((MASK(in_buf[*index],ASN1_TAG)) == ASN1_TAG) {
    /* long form of tag */
    do {
      (*index)++;
      if (*index >= buf_len)
	return ASN1_TAG_ERROR;
      tmp_tag = tmp_tag << 7;
      tmp_tag += (MASK(in_buf[*index],ASN1_LONG_TAG));
    } while (in_buf[*index] >= 128);
    (*index)++;
    tag_no = tag_no + tmp_tag;
  } else {
    tag_no += (MASK(in_buf[*index],ASN1_TAG));
    (*index)++;
  }
  if (*index >= buf_len)
    return ASN1_TAG_ERROR;
  return tag_no;
}


/*
 * int get_value(char *out_buf,unsigned char *in_buf,
 *               int *msg_index,int in_buf_len)
 */
/* assumes next byte/bytes in in_buf is an encoded BER value preceeded by a BER encoded length. Puts value in out_buf.
 */
int get_value(char *out_buf,
	      unsigned char *in_buf,
	      int *msg_index,
	      int in_buf_len)
{
  int len, lenoflen, indef=0, skip_len;
  int ret=0;
  int start_index, out_index = 0;
  
/*   printf("get_value 1\n\r"); */
  if (in_buf[*msg_index] < 0x80){ /* short definite length */
    len = in_buf[*msg_index];
/*     printf("short definite length\r\n"); */
  } else if (in_buf[*msg_index] > 0x80) { /* long definite length */
    lenoflen = (in_buf[*msg_index] & 0x7f); /*length of length */
    len = 0;
    while (lenoflen--) {
      (*msg_index)++;
      len = (len << 8) + in_buf[*msg_index];
    }
    if (len > (in_buf_len - (*msg_index + 1)))
      return ASN1_LEN_ERROR;
  } else 
    indef = 1;
  (*msg_index)++;
/*   printf("get_value 2: len = %d, *msg_index = %d\r\n",len,*msg_index); */
  if (indef == 1) {
    while(!(in_buf[*msg_index]==0 && in_buf[*msg_index + 1]==0)) {
      start_index = *msg_index;
      skip_len = skip_tag(in_buf,msg_index,in_buf_len);
/*       printf("get_value 3: skip_len=%d,start_index=%d,*msg_index=%d\n\r", */
/* 	     skip_len,start_index,*msg_index); */
      memcpy(&out_buf[ret],&in_buf[start_index],skip_len);
      ret += skip_len;
      start_index = *msg_index;
      skip_len = skip_length_and_value(in_buf,msg_index,in_buf_len);
/*       printf("get_value 4: skip_len=%d,start_index=%d,*msg_index=%d\n\r", */
/* 	     skip_len,start_index,*msg_index); */
      memcpy(&out_buf[ret],&in_buf[start_index],skip_len);
      ret += skip_len;
    }
    return ret;
  }
  else
    memcpy(&out_buf[ret],&in_buf[*msg_index],len);
  return len;
}


/*
 * int get_length(unsigned char *in_buf,int *msg_index)
 * assumes next byte/bytes contain a BER encoded length field,
 * which is decoded. The value of the length is returned. If it
 * is an indefinite length the *indef is set to one.
 */
int get_length(unsigned char *in_buf,int *msg_index,
	       int *indef,int in_buf_len)
{
  int len=0, lenoflen;
  
  if (in_buf[*msg_index] < 0x80) /* short definite length */
    len = in_buf[*msg_index];
  else if (in_buf[*msg_index] > 0x80) { /* long definite length */
    lenoflen = (in_buf[*msg_index] & 0x7f); /*length of length */
    len = 0;
    while (lenoflen--) {
      (*msg_index)++;
      len = (len << 8) + in_buf[*msg_index];
    }
    if (len > (in_buf_len - (*msg_index + 1)))
      return ASN1_LEN_ERROR;
  } else 
    *indef = 1;
  (*msg_index)++;
  return len;
}
