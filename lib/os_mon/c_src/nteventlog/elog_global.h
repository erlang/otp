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
#ifndef _ELOG_GLOBAL_H
#define _ELOG_GLOBAL_H
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <process.h>

#define STRICT
#define WIN32_LEAN_AND_MEAN
#include "windows.h"

#ifdef _DEBUG
/*#define HARDDEBUG*/
#define DEBUG
#endif

/*
 * Compile time limits
 */
#define SMALLBUFSIZ 512
#define BIGBUFSIZ 2048
#define MAX_PARAM_STRINGS 200
#define MAX_FACILITY_NAME 100

/*
 * Structure containing message file names
 */
typedef struct _message_files {
  char *event;
  char *param;
} MessageFiles;

/*
 * How to get the default language
 */
#define DEFAULT_LANGID MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT)

#endif /* _ELOG_GLOBAL_H */
