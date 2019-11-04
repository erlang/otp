/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2018-2018. All Rights Reserved.
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
 * ----------------------------------------------------------------------
 *  Purpose : Debug functions for the socket and net NIF(s).
 * ----------------------------------------------------------------------
 *
 */

#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <time.h>

#include <erl_nif.h>
#include "socket_util.h"
#include "socket_dbg.h"

#define TSELF()            enif_thread_self()
#define TNAME(__T__)       enif_thread_name( __T__ )
#define TSNAME()           TNAME(TSELF())

FILE* esock_dbgout = NULL;

extern
void esock_dbg_init(char* filename)
{
  if (filename != NULL) {
    if (strcmp(filename, ESOCK_DBGOUT_DEFAULT) == 0) {
        esock_dbgout = stdout;
    } else if (strcmp(filename, ESOCK_DBGOUT_UNIQUE) == 0) {
        char template[] = "/tmp/esock-dbg-XXXXXX";
        esock_dbgout = fdopen(mkstemp(template), "w+");
    } else {
        esock_dbgout = fopen(filename, "w+");
    }
  } else {
    char template[] = "/tmp/esock-dbg-XXXXXX";
    esock_dbgout = fdopen(mkstemp(template), "w+");
  }
}



/*
 * Print a debug format string *with* both a timestamp and the
 * the name of the *current* thread.
 */
extern
void esock_dbg_printf( const char* prefix, const char* format, ... )
{
  va_list         args;
  char            f[512 + strlen(format)]; // This has to suffice...
  char            stamp[50];
  int             res;

  /*
   * We should really include self in the printout,
   * so we can se which process are executing the code.
   * But then I must change the API....something for later.
   */

  if (esock_timestamp_str(stamp, sizeof(stamp))) {
      res = enif_snprintf(f, sizeof(f), "%s [%s] [%s] %s",
                          prefix, stamp, TSNAME(), format);
  } else {
      res = enif_snprintf(f, sizeof(f), "%s [%s] %s",
                          prefix, TSNAME(), format);
  }
  
  if (res > 0) {
      va_start (args, format);
      enif_vfprintf (esock_dbgout, f, args);
      va_end (args);
      fflush(esock_dbgout);
  }

  return;
}

