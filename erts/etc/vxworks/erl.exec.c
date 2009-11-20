/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
   A simpified version of the 'erl.exec' "startup script".
   Called (e.g. from VxWorks shell) with all arguments in a
   single string, e.g.: erl "-name thisnode -s mymod myfunc".
   These arguments are handled as in 'erl.exec':
   -name
   -sname
   -noshell
   -noinput
   anything else is just passed on to the emulator. Note that there
   is no automatic start of epmd, that -oldshell is implicit, and
   that you need to set current directory appropriately if you want
   auto-load of port programs
*/

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef DEFAULT_HOMEDIR		/* used if environment HOME isn't set */
#define DEFAULT_HOMEDIR "/"
#endif

#define ARGLEN 2048		/* Total length of args passed to erl_main */
#define ARGMAX 64		/* Max no of "extra" args */

static char *erl_cmd = "erl_main -n ";

static toomuch()
{
    fprintf(stderr, "erl: Too many arguments\n");
    return(-1);
}

static toolittle(arg)
char *arg;
{
    fprintf(stderr, "erl.exec: Missing argument for %s\n", arg);
    return(-1);
}

erl_exec(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
int arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10;
{
    char *shell = "-oldshell ", *noshell = "",
       *home, *rootdir, *bindir, *progname;
    char cmd[ARGLEN], eargs[ARGLEN], iargs[ARGLEN];
    char *args[ARGMAX], *arglast = NULL, *argp;
    int nargs = 0, len, i;

    if ((rootdir = getenv("ROOTDIR")) == NULL || 
	(bindir = getenv("BINDIR")) == NULL ||
	(progname = getenv("PROGNAME")) == NULL) {
      fprintf(stderr, "erl.exec: ROOTDIR, BINDIR, and PROGNAME must be set.");
      return -1;
    }
    eargs[0] = '\0';
    iargs[0] = '\0';
    if ((home = getenv("HOME")) == NULL)
	home = DEFAULT_HOMEDIR;
    argp = strtok_r((char *)arg1, " \t", &arglast);
    while (argp != NULL) {
      if (strcmp(argp, "-name") == 0) {
	if ((argp = strtok_r((char *)NULL, " \t", &arglast)) == NULL)
	  return(toolittle("-name"));
	strcat(iargs, "-name ");
	strcat(iargs, argp);
	strcat(iargs, " ");
      } else if (strcmp(argp, "-sname") == 0) {
	if ((argp = strtok_r((char *)NULL, " \t", &arglast)) == NULL)
	  return(toolittle("-sname"));
	strcat(iargs, "-sname ");
	strcat(iargs, argp);
	strcat(iargs, " ");
      } else if (strcmp(argp, "-noshell") == 0) {
	strcat(iargs, "-noshell -noinp_shell ");
      } else if (strcmp(argp, "-noinput") == 0) {
	strcat(iargs, "-noshell -noinput ");
      } else {
	if (nargs > ARGMAX - 1)
	  return(toomuch());
	args[nargs++] = argp;
      }
      argp = strtok_r((char *)NULL, " \t", &arglast);
    }
    strcpy(cmd, erl_cmd);
    strcat(cmd, eargs);
    strcat(cmd, " -- -root ");
    strcat(cmd, rootdir);
    strcat(cmd, " -progname ");
    strcat(cmd, progname); 
    strcat(cmd, " -- ");
    strcat(cmd, "-home ");
    strcat(cmd, home);
    strcat(cmd, " ");
    strcat(cmd, iargs);

    len = strlen(cmd);
    for (i = 0; i < nargs; i++) {
	if (len + strlen(args[i]) + 2 >= ARGLEN)
	    return(toomuch());
	cmd[len++] = ' ';
	strcpy(&cmd[len], args[i]);
	len += strlen(args[i]);
    }
    argcall(cmd);
}

