/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2018-2020. All Rights Reserved.
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
BOOLEAN_T esock_dbg_init(char* filename)
{
    size_t n;
    FILE *fp;
    const char mode[] = "w+";

    if (filename == NULL) {
        esock_dbgout = stdout;
        return TRUE; // Valid default stdout
    }

    if ((n = strlen(filename)) == 0) {
        esock_dbgout = stdout;
        return TRUE; // Valid selection of stdout
    }

    fp = NULL;

    /* If there is trailing ?????? replace it with XXXXXX
     * and use mkstemp() to create an unique file name
     */
    if (n >= 6) {
        size_t k;

        for (k = n - 6;  k < n;  k++)
            if (filename[k] != '?') break;

        if (k == n) {
            int fd;
            /* ?:s up to the end */

            for (k = n - 6;  k < n;  k++)
                filename[k] = 'X';

            if ((fd = mkstemp(filename)) >= 0)
                fp = fdopen(fd, mode);
        } else {
            fp = fopen(filename, mode);
        }
    } else {
        fp = fopen(filename, mode);
    }

    if (fp != NULL) {
        esock_dbgout = fp;
        return TRUE; // Succesful file open
    }

    esock_dbgout = stdout;
    return FALSE; // Selected file did not work
}



/*
 * Print a debug format string *with* both a timestamp and the
 * the name of the *current* thread.
 */
extern
void esock_dbg_printf( const char* prefix, const char* format, ... )
{
  va_list         args;
  char            f[512]; // This has to suffice...
  char            stamp[64];
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
  
  if (res < sizeof(f)) {
      va_start (args, format);
      enif_vfprintf(esock_dbgout, f, args);
      va_end (args);

      fflush(esock_dbgout);
  }
}

