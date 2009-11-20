/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
#include <ic.h>


int oe_ei_encode_ulonglong(CORBA_Environment *ev, CORBA_unsigned_long_long p) {
  int size = ev->_iout + __OE_ULONGLONGSZ__;

  if (size >= ev->_outbufsz) {
    char *buf = ev->_outbuf;
    int bufsz = ev->_outbufsz + ev->_memchunk;
    
    if ((buf = realloc(buf,bufsz)) != NULL) {
      ev->_outbuf = buf;
      ev->_outbufsz += ev->_memchunk;
    }
    else {
      CORBA_exc_set(ev, CORBA_SYSTEM_EXCEPTION, NO_MEMORY, "End of heap memory while encoding");
      return -1;  /* OUT OF MEMORY */ 
    }
  }
  
  /* CORBA_long_long = long because of erl_interface limitation */
  return ei_encode_ulong(ev->_outbuf, &ev->_iout, p);
}


