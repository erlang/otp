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
