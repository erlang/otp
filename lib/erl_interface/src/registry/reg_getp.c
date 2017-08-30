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
#include "reg.h"

const void *ei_reg_getpval(ei_reg *reg, const char *key, int *size)
{
  ei_hash *tab;
  ei_reg_obj *obj;

  if (!key || !reg) return NULL;
  tab = reg->tab;
  
  if ((!(obj=ei_hash_lookup(tab,key))) || /* return (const void *)EI_NOTFOUND; */
      (obj->attr & EI_DELET)  || /* return (const void *)EI_NOTFOUND; */
      (ei_reg_typeof(obj) != EI_BIN)) /* return (const void *)EI_TYPE; */
    return NULL;

  if (size) *size=obj->size;
  return obj->val.p;
}
