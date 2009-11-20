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
#ifndef _EIEXT_H
#define _EIEXT_H

/* FIXME maybe put into eidef.h */

#define ERL_VERSION_MAGIC 131   /* 130 in erlang 4.2 */

/* from erl_eterm.h */
#define ERL_MAX ((1 << 27)-1)
#define ERL_MIN -(1 << 27)

/* FIXME we removed lots of defines, maybe some C files don't need to include
   this header any longer? */

#endif /* _EIEXT_H */
