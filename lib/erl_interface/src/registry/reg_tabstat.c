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
#include "reg.h"

/* get table information */
int ei_reg_tabstat(ei_reg *reg, struct ei_reg_tabstat *obuf)
{
  ei_hash *tab;
  
  if (!reg || !obuf) return -1; /*  return EI_BADARG; */
  tab = reg->tab;
  
  obuf->npos = tab-> npos;
  obuf->size = tab->size;
  obuf->nelem = tab->nelem;
  obuf->collisions = tab->nelem - tab->npos;

  return 0;
}
