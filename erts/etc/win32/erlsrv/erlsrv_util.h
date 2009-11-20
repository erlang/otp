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
#ifndef _ERLSRV_UTIL_H
#define _ERLSRV_UTIL_H

extern char *service_name;
extern char *real_service_name;
void log_warning(char *mess);
void log_error(char *mess);
void log_info(char *mess);

char *envdup(char *env);
/*
** Call before env_to_arg to get a 'freeable' environment block.
*/

char *arg_to_env(char **arg);
/*
** Frees the argument list before returning!
*/

char **env_to_arg(char *env);
/*
** Frees the environment block before returning!
*/


#ifndef NDEBUG
void log_debug(char *mess);
#else
#define log_debug(mess) /* Debug removed */
#endif

#endif /* _ERLSRV_UTIL_H */
