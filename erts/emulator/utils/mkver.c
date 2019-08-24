/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
/*
 * Makes the file erl_version.h.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <time.h>

int
main(argc, argv)
int argc;
char** argv;
{
    FILE *file;
#if ERTS_SAVED_COMPILE_TIME
    time_t now;
#endif
    char *cnow = "";

    if (argc != 2) {
	fprintf(stderr, "usage: mkver version\n");
	exit(1);
    }

    if ((file = fopen("erl_version.h", "wb")) == NULL) {
	fprintf(stderr, "Could not create file 'erl_version.h'!\n");
	exit(1);
    }

#if ERTS_SAVED_COMPILE_TIME
    time(&now);
    cnow = ctime(&now);
    cnow[24] = '\0';		/* tidelipom */
#endif
    fprintf(file, "/* This file was created by mkver -- don't modify.*/\n");
    fprintf(file, "#define ERLANG_VERSION \"%s\"\n", argv[1]);
    fprintf(file, "#define ERLANG_COMPILE_DATE \"%s\"\n", cnow);
    fclose(file);

    exit(0);
    return 0;
}
