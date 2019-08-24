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

