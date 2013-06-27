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

extern wchar_t *service_name;
extern wchar_t *real_service_name;
void log_warning(wchar_t *mess);
void log_error(wchar_t *mess);
void log_info(wchar_t *mess);

wchar_t *envdup(wchar_t *env);
/*
** Call before env_to_arg to get a 'freeable' environment block.
*/

wchar_t *arg_to_env(wchar_t **arg);
/*
** Frees the argument list before returning!
*/

wchar_t **env_to_arg(wchar_t *env);
/*
** Frees the environment block before returning!
*/


#ifndef NDEBUG
void log_debug(wchar_t *mess);
#else
#define log_debug(mess) /* Debug removed */
#endif

#endif /* _ERLSRV_UTIL_H */
