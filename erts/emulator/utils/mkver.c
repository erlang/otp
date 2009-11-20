/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
    time_t now;
    char *cnow;

    if (argc != 2) {
	fprintf(stderr, "usage: mkver version\n");
	exit(1);
    }

    if ((file = fopen("erl_version.h", "wb")) == NULL) {
	fprintf(stderr, "Could not create file 'erl_version.h'!\n");
	exit(1);
    }

    time(&now);
    cnow = ctime(&now);
    cnow[24] = '\0';		/* tidelipom */
    fprintf(file, "/* This file was created by mkver -- don't modify.*/\n");
    fprintf(file, "#define ERLANG_VERSION \"%s\"\n", argv[1]);
    fprintf(file, "#define ERLANG_COMPILE_DATE \"%s\"\n", cnow);
    fclose(file);

    exit(0);
    return 0;
}
