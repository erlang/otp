/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
#ifndef _ERL_START_H
#define _ERL_START_H

#define ERL_START_MSG "gurka" /* make something up */
#define ERL_START_TIME 10000   /* wait this long (ms) */
#define ERL_START_LOGFILE ".erl_start.out" /* basename of logfile */

/* flags used by erl_connect and erl_xconnect */
#define ERL_START_ENODE   0x0001
#define ERL_START_EPMD    0x0002
#define ERL_START_LONG    0x0004
#define ERL_START_COOKIE  0x0008
#define ERL_START_DEBUG   0x0010
#define ERL_START_VERBOSE 0x0020
#define ERL_START_REMOTE  0x0040

/* error return values */
#define ERL_S_TIMEOUT    -51  /* a timeout occurred */
#define ERL_BADARG     -52  /* an argument contained an incorrect value */
#define ERL_SYS_ERROR  -99  /* a system error occurred (check errno) */

/* start an erlang system */
int erl_start_sys(ei_cnode *ec, char *alive, Erl_IpAddr addr, int flags,
		  char *erl, char *add_args[]);

#endif /* _ERL_START_H */
