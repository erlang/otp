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
#ifdef VXWORKS
#include <vxWorks.h>
#endif

#include <stdarg.h>
#include "reg.h"

int ei_reg_setval(ei_reg *reg, const char *key, int flags, ...)
{
  va_list ap;
  int rval = 0;
  
  if (!key || !reg) return -1; /* return EI_BADARG; */

  va_start(ap,flags);

  switch (flags & EI_REG_TYPEMASK) {
  case EI_INT: {
    long i;

    i = va_arg(ap,long);
    rval = ei_reg_setival(reg,key,i);
    break;
  }
  case EI_FLT: {
    double f;

    f = va_arg(ap,double);
    rval = ei_reg_setfval(reg,key,f);
    break;
  }
  case EI_STR: {
    char *s;
    
    s = va_arg(ap,char*);
    rval = ei_reg_setsval(reg,key,s);
    break;
  }
  case EI_BIN: {
    void *p;
    int len;

    p = va_arg(ap,void*);
    len = va_arg(ap,int); 
    rval = ei_reg_setpval(reg,key,p,len);
    break;
  }

  default:
    rval = -1;
    /* rval = EI_BADARG; */
  }

  /* clean up & return */
  va_end(ap);
  return rval;
}

