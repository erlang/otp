/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2016. All Rights Reserved.
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


int oe_ei_encode_port(CORBA_Environment *ev, const erlang_port *p) {
  int size = ev->_iout;
  
  ei_encode_port(NULL, &size, p);

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
    
  return ei_encode_port(ev->_outbuf, &ev->_iout, p);
}

