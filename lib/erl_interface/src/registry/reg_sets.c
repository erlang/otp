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
#include <string.h>
#include "reg.h"

int ei_reg_setsval(ei_reg *reg, const char *key, const char *s)
{
  ei_hash *tab;
  ei_reg_obj *obj=NULL;
  int len = 0;

  
  if (!key || !reg) return -1; /* return EI_BADARG; */
  tab = reg->tab;
  if (s) len = strlen(s) +1;

  if ((obj=ei_hash_lookup(tab,key))) {
    /* object with same name already exists */
    switch (ei_reg_typeof(obj)) {
    case EI_INT:
      break;
    case EI_FLT:
      break;
    case EI_STR:
      if (obj->size > 0) free(obj->val.s);
      break;
    case EI_BIN:
      if (obj->size > 0) free(obj->val.p);
      break;
    default:
      return -1;
      /* return EI_UNKNOWN; */
    }
  }
  else {
    /* object is new */
    if (!(obj=ei_reg_make(reg,EI_STR))) return -1; /* return EI_NOMEM; */
    ei_hash_insert(tab,key,obj);
  }
  
  obj->attr = EI_STR | EI_DIRTY;
  obj->val.s=(char *)s;
  obj->size = len;

  return 0;
}
