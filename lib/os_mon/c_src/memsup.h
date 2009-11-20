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
 */
/*
 * This header defines the protocol between the erlang 
 * memsup module and the C module.
 */
#ifndef _SYSMEM_H
#define _SYSMEM_H

/* Simple memory statistics */
/*IG*/ #define SHOW_MEM 1 

/* Extended memory statistics */
/*IG*/ #define SHOW_SYSTEM_MEM 2

/* Tags for the extended statistics */
/*IG*/ #define SHOW_SYSTEM_MEM_END 0
/*IG*/ #define MEM_SYSTEM_TOTAL 1
/*IG*/ #define MEM_TOTAL 2
/*IG*/ #define MEM_FREE 3
/*IG*/ #define MEM_LARGEST_FREE 4
/*IG*/ #define MEM_NUMBER_OF_FREE 5
/*Extension*/
/*IG*/ #define MEM_BUFFERS 6
/*IG*/ #define MEM_CACHED 7
/*IG*/ #define MEM_SHARED 8
/*IG*/ #define SWAP_TOTAL 9
/*IG*/ #define SWAP_FREE 10

#endif
