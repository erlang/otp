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

/* open a registry */
ei_reg *ei_reg_open(int size)
{
  ei_reg *new;
  
  if (size <= 0) return NULL;

  if (!(new = malloc(sizeof(*new)))) return NULL;

  new->freelist = NULL;
  
  if (!(new->tab = ei_hash_newtab(size))) {
    free(new);
    return NULL;
  }

  return new;
}
