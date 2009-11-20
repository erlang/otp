/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
#include "eidef.h"
#include "ei_trace.h"

/* this is our lamport clock */
erlang_trace *ei_trace(int query, erlang_trace *token)
{
  /* FIXME problem for threaded ? */ 
  static erlang_trace save_token;
  static int tracing = 0;
  static int clock = 0;

  
  switch (query) {
  case -1: /* we are no longer tracing */
    tracing = 0;
    break;
    
  case 0: /* are we tracing? */
    if (tracing) {
      clock++;
      save_token.prev = save_token.serial++;
      return &save_token;
    }
    break;
    
  case 1: /* we are now tracing */
    tracing = 1;
    save_token = *token;
    if (save_token.serial > clock) 
      save_token.prev = clock = token->serial;
    break;
  }

  return NULL;
}

