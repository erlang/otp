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

#include <stdlib.h>
#include "rmod_random__s.h"


rmod_random_produce__rs* 
rmod_random_produce__cb(rmod_random oe_obj, double *rs,  CORBA_Environment *oe_env)

{
  *rs = (double) rand();

  return (rmod_random_produce__rs*) NULL;
}
 

rmod_random_init__rs* 
rmod_random_init__cb(rmod_random oe_obj,  long* seed1, long* seed2, long* seed3, CORBA_Environment *oe_env)

{
  srand(*seed1 * *seed2 * *seed3);
  
  return (rmod_random_init__rs*) NULL;
}



