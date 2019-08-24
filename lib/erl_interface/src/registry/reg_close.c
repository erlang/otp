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

/* really remove an object (help function to hash_freetab) */
static void obj_free(void *p)
{
  ei_reg_obj *obj = p;
  
  if (obj) {
    switch (ei_reg_typeof(obj)) {
    case EI_STR:
      free(obj->val.s);
      break;

    case EI_BIN:
      free(obj->val.p);
      break;
    }

    /* really remove the inode (don't use freelist here) */
    free(obj);
  }
  return;
}

/* close an open registry */
int ei_reg_close(ei_reg *reg)
{
  ei_reg_obj *obj, *next;
  
  if (!reg) return -1; /* return EI_BADARG; */
  
  /* remove hash_table */
  ei_hash_freetab(reg->tab,obj_free);

  /* remove freelist */
  obj = reg->freelist;
  while (obj) {
    next = obj->next;
    free(obj);
    obj = next;
  }

  /* remove registry */
  free(reg);

  return 0;
}
