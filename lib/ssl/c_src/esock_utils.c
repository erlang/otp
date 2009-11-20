/*
 *  %CopyrightBegin%
 *  
 *  Copyright Ericsson AB 1999-2009. All Rights Reserved.
 *  
 *  The contents of this file are subject to the Erlang Public License,
 *  Version 1.1, (the "License"); you may not use this file except in
 *  compliance with the License. You should have received a copy of the
 *  Erlang Public License along with this software. If not, it can be
 *  retrieved online at http://www.erlang.org/.
 *  
 *  Software distributed under the License is distributed on an "AS IS"
 *  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 *  the License for the specific language governing rights and limitations
 *  under the License.
 *  
 *  %CopyrightEnd%
 */

/*
 * Purpose: Safe memory allocation and other utilities.
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "esock_utils.h"

static char *strtok_quote(char *s1, const char *s2);


void *esock_malloc(size_t size)
{
    void *p;

    p = malloc(size);
    if (!p) {
	fprintf(stderr, "esock_malloc: cannot alloc %d bytes\n", size);
	exit(EXIT_FAILURE);
    }
    return p;
}

void *esock_realloc(void *p, size_t size)
{
    void *np;

    np = realloc(p, size);
    if (!np) {
	fprintf(stderr, "esock_realloc: cannot realloc %d bytes\n", size);
	exit(EXIT_FAILURE);
    }
    return np;
}

void esock_free(void *p)
{
    free(p);
}

/* Builds an argv array from cmd. Spaces and tabs within double quotes
 * are not considered delimiters. Double quotes are removed.
 * 
 * The return value is argc, and the pointer to char ** is set. argc 
 * is non-negative, argv[0], ..., argv[argc - 1] are pointers to
 * strings, and argv[argc] == NULL.  All argv[0], ..., argv[argc - 1]
 * must be freed by the user, and also the argv pointer itself. 
 *
 * Example: cmd = abc"/program files/"olle nisse, results in 
 * argv[0] = abc/program files/olle, argv[1] = nisse, argc = 2.
 *
 */
int esock_build_argv(char *cmd, char ***argvp)
{
    int argvsize = 10, argc = 0;
    char *args, *tokp, *argp;
    char **argv;

    argv = esock_malloc(argvsize * sizeof(char *));
    args = esock_malloc(strlen(cmd) + 1);
    strcpy(args, cmd);
    tokp = strtok_quote(args, " \t");
    while (tokp != NULL) {
	if (argc + 1 >= argvsize) {
	    argvsize += 10;
	    argv = esock_realloc(argv, argvsize * sizeof(char *));
	}
	argp = esock_malloc(strlen(tokp) + 1);
	strcpy(argp, tokp);
	argv[argc++] = argp;
	tokp = strtok_quote(NULL, " \t");
    }
    esock_free(args);
    argv[argc] = NULL;
    *argvp = argv;
    return argc;
}

/* strtok_quote
 * Works as strtok, but characters within pairs of double quotes are not 
 * considered as delimiters. Quotes are removed.
 */
static char *strtok_quote(char *s1, const char *s2)
{
    static char *last;
    char *s, *t, *u;

    s = (s1) ? s1 : last;
    if (!s) 
	return last = NULL;

    while (*s != '"' && *s != '\0' && strchr(s2, *s))
	s++;
    t = s;

    while (1) {
	if (*t == '"') {
	    t++;
	    while (*t != '"' && *t != '\0')
		t++;
	    if (*t == '\0') {
		last = NULL;
		goto end;
	    }
	    t++;
	}
	while(*t != '"' && *t != '\0' && !strchr(s2, *t))
	    t++;
	if (*t == '\0') {
	    last = NULL;
	    goto end;
	} else if (*t != '"') {
	    *t = '\0';
	    last = t + 1;
	    goto end;
	}
    }
end:
    /* Remove quotes */
    u = t = s;
    while (*u) {
	if (*u == '"')
	    u++;
	else 
	    *t++ = *u++;
    }
    *t = '\0';
    return s;
}

