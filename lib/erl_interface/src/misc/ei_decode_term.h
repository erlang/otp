/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2001-2009. All Rights Reserved.
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

#ifndef _EI_DECODE_TERM_H
#define _EI_DECODE_TERM_H

/* Returns 1 if term is decoded, 0 if term is OK, but not decoded here
   and -1 if something is wrong.
   ONLY changes index if term is decoded (return value 1)! */

int ei_decode_ei_term(const char* buf, int* index, ei_term* term);

#endif /* _EI_DECODE_TERM_H */
