/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2009. All Rights Reserved.
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
 * The protocol version number used between to_erl and run_erl.
 */
#define RUN_ERL_HI_VER 1  /* My preferred protocol version */
#define RUN_ERL_LO_VER 0  /* The lowest version I accept to talk with */

/* Version history:
 * 0: Older, without version handshake
 * 1: R12B-3, version handshake + window size ctrl
 */
