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
#ifndef _ERLSRV_SERVICE_H
#define _ERLSRV_SERVICE_H

#define CYCLIC_RESTART_LIMIT 10 /* Seconds */
#define SUCCESS_WAIT_TIME (10*1000) /* Wait 5 s before reporting a service
				as really started */
#define NO_SUCCESS_WAIT 0
#define INITIAL_SUCCESS_WAIT 1
#define RESTART_SUCCESS_WAIT 2


int service_main(int argc, wchar_t **argv);

#endif /* _ERLSRV_SERVICE_H */
