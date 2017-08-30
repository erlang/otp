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

static ei_bucket *do_purge(ei_reg *reg, int i)
{
  ei_hash *tab = reg->tab;
  ei_bucket *head = tab->tab[i];
  ei_bucket *this, *next;
  ei_reg_obj *obj;

  /* first position special case */
  while ((this=head)) {
    obj = (ei_reg_obj*)(this->value); /* cast to eliminate 'const' warning */
    if (obj->attr & EI_DELET) {
      head = this->next;
      ei_reg_free(reg,obj); /* free obj to freelist */
      ei_hash_bfree(tab,this); /* free bucket to freelist */
      tab->nelem--;
    }
    else break;
  }

  /* check remaining positions */
  this = head;
  while (this && this->next) {
    next = this->next;
    obj = (ei_reg_obj*)(next->value); /* cast to eliminate 'const' warning */
    if (obj->attr & EI_DELET) {
      this->next = next->next;
      ei_reg_free(reg,obj); /* free object to freelist */
      ei_hash_bfree(tab,next); /* free bucket to freelist */
      tab->nelem--;
    }
    else this = this->next; 
  }
  
  return head;
}

int ei_reg_purge(ei_reg *reg)
{
  ei_hash *tab;
  int i;

  if (!reg) return -1;
  tab = reg->tab;

  for (i=0;i<tab->size;i++) {
    if ((tab->tab[i])) {
      tab->tab[i] = do_purge(reg,i);
      if (!tab->tab[i]) tab->npos--;
    }
  }

  return 0;
}
