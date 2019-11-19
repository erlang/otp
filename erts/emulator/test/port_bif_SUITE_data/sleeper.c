/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020. All Rights Reserved.
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

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#ifndef __WIN32__
#  include <unistd.h>
#  include <sys/time.h>
#else
#  include "windows.h"
#  include "winbase.h"
#endif
#include <sys/types.h>

int
main(int argc, char *argv[])
{
    long int ms;
    char *endp;
    
    if (argc != 2) {
        fprintf(stderr, "Invalid argument count: %d\n", argc);
        exit(1);
    }

    errno = 0;
    ms = strtol(argv[1], &endp, 10);
    if (errno || argv[1] == endp || *endp != '\0' || ms < 0) {
        if (errno == 0)
            errno = EINVAL;
        perror("Invalid timeout value");
        exit(1);
    }
    
#ifdef __WIN32__
    Sleep(ms);
#else
    {
        struct timeval t;
        t.tv_sec = ms/1000;
        t.tv_usec = (ms % 1000) * 1000;

        select(0, NULL, NULL, NULL, &t);
    }
#endif
  
  return 0;
}
