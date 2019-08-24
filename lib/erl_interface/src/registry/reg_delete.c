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

int ei_reg_delete(ei_reg *reg, const char *key) 
{
  ei_hash *tab;
  ei_reg_obj *obj;

  if (!key || !reg) return -1; /* return EI_BADARG; */
  tab = reg->tab;
  if (!(obj = ei_hash_lookup(tab,key))) return -1; /* return EI_NOTFOUND; */

  /* just mark the object deleted */
  obj->attr |= (EI_DELET | EI_DIRTY);
  
  return 0;
}
