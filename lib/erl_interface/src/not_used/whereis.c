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
#ifdef __WIN32__
#include <winsock2.h>
#include <windows.h>
#include <winbase.h>

#elif VXWORKS
#include <unistd.h>

#else /* unix */
#include <unistd.h>

#endif

#include <string.h>
#include <stdlib.h>
#include "erl_interface.h"
#include "erl_connect.h"
#include "erl_format.h"
#include "erl_eterm.h"
#include "erl_malloc.h"

/* FIXME rewrite to ei functions */
/* FIXME not used */

erlang_pid *erl_whereis(int fd, const char *name)
{
  ETERM *reply;
  ETERM *n;
  /* FIXME problem for threaded ? */
  static erlang_pid pid;
  
  n = erl_format("[~a]",name);
  reply = erl_rpc(fd,"erlang","whereis",n);
  erl_free_term(n);

  if (reply && (ERL_IS_PID(reply))) {
    char *node;
    node =  ERL_PID_NODE(reply);
    strcpy(pid.node,node);
    pid.num = ERL_PID_NUMBER(reply);
    pid.serial = ERL_PID_SERIAL(reply);
    pid.creation = ERL_PID_CREATION(reply);
    erl_free_term(reply);
    return &pid;
  }

  if (reply) erl_free_term(reply);
  return NULL;
}

