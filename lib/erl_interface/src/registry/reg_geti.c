/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2009. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * %CopyrightEnd%
 *

 */
#include "reg.h"

long ei_reg_getival(ei_reg *reg, const char *key)
{
  ei_hash *tab;
  ei_reg_obj *obj=NULL;

  if (!key || !reg) return -1; /* return EI_BADARG; */
  tab = reg->tab;
  if (!(obj=ei_hash_lookup(tab,key))) return -1; /* return EI_NOTFOUND; */
  if (obj->attr & EI_DELET) return -1; /* return EI_NOTFOUND; */
  if (ei_reg_typeof(obj) != EI_INT) return -1; /* return EI_TYPE; */

  return obj->val.i;
}
