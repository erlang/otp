/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
