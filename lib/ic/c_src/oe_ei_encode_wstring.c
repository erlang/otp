/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1999-2016. All Rights Reserved.
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


int oe_ei_encode_wstring(CORBA_Environment *ev, CORBA_wchar *p) {

  int len,wchar,size,tmp,error_code;

  len = ic_wstrlen(p);
  size = ev->_iout + __OE_LISTHDRSZ__ +(len * __OE_WCHARSZ__); 

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

  /* Encode the wide string */
  error_code = 0;

  if ((error_code = oe_ei_encode_list_header(ev, len)) < 0)
    return error_code;
  
  for(tmp = 0; tmp < len; tmp++) 
    if ((error_code = oe_ei_encode_wchar(ev, p[tmp])) < 0)
      return error_code;

  if ((error_code = oe_ei_encode_empty_list(ev)) < 0)
    return error_code;

  return 0;
}


