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
#ifndef _ERL_GLOBAL_H
#define _ERL_GLOBAL_H

char **erl_global_names(int fd, int *count);
ETERM *erl_global_whereis(int fd, const char *name, char *node);
int erl_global_register(int fd, const char *name, ETERM *pid);
int erl_global_unregister(int fd, const char *name);

#endif /* _ERL_GLOBAL_H */
