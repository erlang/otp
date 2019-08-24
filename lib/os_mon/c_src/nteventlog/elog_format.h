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
#ifndef _ELOG_FORMAT_H
#define _ELOG_FORMAT_H
/*
 * Module: elog_format
 * Purpouse: Format messages in the eventlog.
 * ToDo: Maximum buffersize is used...
 */

#include "elog_global.h"

char *format_message(MessageFiles mf, DWORD id, 
		     char *strings, int numstrings, 
		     char *buff, int bufflen);
/*
 * Formats an eventlog message with the messagefiles
 * in mf, the ID id, the stringarray strings, 
 * containing numstrings strings into buff.
 * if bufflen is to small or anything else failes, 
 * the return value is NULL.
 */

#endif /* _ELOG_FORMAT_H */
