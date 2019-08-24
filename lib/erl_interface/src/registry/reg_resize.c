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

/* resize a registry - return the new size or -1 on error */
int ei_reg_resize(ei_reg *reg, int newsize)
{
  ei_hash *newtab=NULL;

  if (!reg) return -1;
  if (newsize <= 0) return -1;
  
  if ((newtab=ei_hash_resize(reg->tab,newsize))) {
    reg->tab = newtab;
  }

  return reg->tab->size;
}
