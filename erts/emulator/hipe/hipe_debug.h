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
 */
/* $Id$
 * hipe_debug.h
 */
#ifndef HIPE_DEBUG_H
#define HIPE_DEBUG_H

extern void hipe_print_estack(Process *p);
extern void hipe_print_heap(Process *p);
extern void hipe_print_pcb(Process *p);

#endif /* HIPE_DEBUG_H */
