/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2013. All Rights Reserved.
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
#ifndef ERL_TO_ERL_H
#define ERL_TO_ERL_H

#define TO_ERL_USAGE "to_erl [-h|-F] %s\n"			\
  "\t-h\tThis help text.\n"						\
  "\t-f\tForce connection even though pipe is locked by other to_erl process."

int to_erl(int argc, char **argv);

#endif
