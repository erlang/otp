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



