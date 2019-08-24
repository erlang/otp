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


/* make a new ei_reg_obj object. If the freelist for this registry is
 * not empty, an object will be returned from there. Otherwise one
 * will be created with malloc().
 */
ei_reg_obj *ei_reg_make(ei_reg *reg, int attr)
{
  ei_reg_obj *new=NULL;

  if (reg->freelist) {
    new = reg->freelist;
    reg->freelist = new->next;
    /* fprintf(stderr,"%s:%d: found %p on freelist\n",__FILE__,__LINE__,new); */
  }
  else {
    new = malloc(sizeof(*new));
    /* fprintf(stderr,"%s:%d: allocated %p\n",__FILE__,__LINE__,new); */
  }

  if (new) {
    new->attr=attr | EI_DIRTY;
    new->size=0;
    new->next = NULL;
  }
  return new;
}
