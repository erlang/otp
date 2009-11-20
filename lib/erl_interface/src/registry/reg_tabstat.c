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

/* get table information */
int ei_reg_tabstat(ei_reg *reg, struct ei_reg_tabstat *obuf)
{
  ei_hash *tab;
  
  if (!reg || !obuf) return -1; /*  return EI_BADARG; */
  tab = reg->tab;
  
  obuf->npos = tab-> npos;
  obuf->size = tab->size;
  obuf->nelem = tab->nelem;
  obuf->collisions = tab->nelem - tab->npos;

  return 0;
}
