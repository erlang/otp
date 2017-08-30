/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
#include <ic.h>


int oe_encode_erlang_binary(CORBA_Environment *ev, erlang_binary *binary) {

  int size = ev->_iout;
    
  ei_encode_binary(0, &size, binary->_buffer, binary->_length);

  if (size >= ev->_outbufsz) {
    char *buf = ev->_outbuf;
    int bufsz = ev->_outbufsz + ev->_memchunk;
    
    while (size >= bufsz)
      bufsz += ev->_memchunk;
    
    if ((buf = realloc(buf, bufsz)) == NULL) {
      CORBA_exc_set(ev, CORBA_SYSTEM_EXCEPTION, NO_MEMORY, "End of heap memory while encoding");
      return -1;  /* OUT OF MEMORY */ 
    }

    ev->_outbuf = buf;
    ev->_outbufsz = bufsz;
  }

  return ei_encode_binary(ev->_outbuf, &ev->_iout, binary->_buffer, binary->_length);
}



int oe_sizecalc_erlang_binary(CORBA_Environment *ev, int* _index, int* _size) {

    long _malloc_size = 0;
    int _error = 0;

    if(*_size == 0)
	*_size = ((*_size + sizeof(erlang_binary))+sizeof(double)-1)&~(sizeof(double)-1);
    
    if ((_error = ei_decode_binary(ev->_inbuf, _index, 0, &_malloc_size)) < 0)
	return _error;	
    
    *_size = ((*_size + (int)_malloc_size)+sizeof(double)-1)&~(sizeof(double)-1);

    return 0;
}


int oe_decode_erlang_binary(CORBA_Environment *ev, char *_first, int* _index, erlang_binary *binary) {

    long _length = 0;
    int _error = 0;

    if((char*) binary == _first)
	*_index = ((*_index + sizeof(erlang_binary))+sizeof(double)-1)&~(sizeof(double)-1);

    binary->_buffer = (CORBA_octet *)(_first+*_index);
  
    if ((_error = ei_decode_binary(ev->_inbuf, &ev->_iin, binary->_buffer, &_length)) < 0)
	return _error;

    binary->_length = (CORBA_unsigned_long)_length;
    
    *_index = ((*_index)+_length+sizeof(double)-1)&~(sizeof(double)-1);
    
    return 0;
}



int print_erlang_binary(erlang_binary *binary) {

    int i=0;

    if (binary == NULL)
	return -1;

    fprintf(stdout,"binary->_length : %ld\n",binary->_length);
    fprintf(stdout,"binary->_buffer : "); 
    if(binary->_buffer != NULL) {
	for (i=0; i<binary->_length; i++)
	    fprintf(stdout,"%c",binary->_buffer[i]);
	fprintf(stdout,"\n"); 
    } else
	fprintf(stdout,"NULL\n"); 
    return 0;
}
