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
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <vxWorks.h>
#include <symLib.h>
#include <sysSymTbl.h>
#include <a_out.h>

extern char *malloc();
static STATUS lookup();

/*
   Little utility to convert from Unix' argv,argv calling conventions to
   VxWorks' arg0,arg1,arg2,...
   Will do limited argument parsing - no parenthesis around nor commas
   between the args, which may be "-enclosed strings (without \ escapes),
   '-enclosed characters (also no \ escapes), integers, or symbols.
*/

int vxcall(argc, argv)
int argc;
char **argv;
{
    int vxarg[10];		/* Max 10 args can be passed */
    FUNCPTR entry;
    SYM_TYPE type;
    int i, l;

#ifdef DEBUG
    fdprintf(2, "vxcall:");
    for (i = 1; i < argc; i++)
	fdprintf(2, " %s", argv[i]);
    fdprintf(2, "\n");
#endif
    if (lookup(argv[1], N_EXT | N_TEXT, (char **)&entry) != OK)
	return(ERROR);
    /* Do limited "C" parsing of the args */
    for (i = 0; i < 10; i++) {
	if (i < argc - 2) {
	    switch (argv[i+2][0]) {
	    case '"':
		l = strlen(argv[i+2]) - 1;
		if (argv[i+2][l] != '"')
		    return(ERROR);
		/* just strip the quotes - should do \escapes within... */
		vxarg[i] = (int)&argv[i+2][1];
		argv[i+2][l] = '\0';
		break;
	    case '\'':
		if (argv[i+2][2] != '\'')
		    return(ERROR);
		vxarg[i] = argv[i+2][1]; /* should do \escapes... */
		break;
	    case '1': case '2': case '3': case '4':
	    case '5': case '6': case '7': case '8': case '9':
		vxarg[i] = atoi(argv[i+2]); /* should do octal, hex, float.. */
		break;
	    default:
		if (lookup(argv[i+2], 0, (char **)&vxarg[i]) != OK)
		    return(ERROR);
	    }
	} else
	    vxarg[i] = 0;
    }
#ifdef DEBUG
    fdprintf(2, "calling 0x%x(0x%x,0x%x,0x%x,0x%x,0x%x,0x%x,0x%x,0x%x,0x%x,0x%x)\n",
	     entry, vxarg[0], vxarg[1], vxarg[2], vxarg[3], vxarg[4],
		    vxarg[5], vxarg[6], vxarg[7], vxarg[8], vxarg[9]);
#endif
    return((*entry)(vxarg[0], vxarg[1], vxarg[2], vxarg[3], vxarg[4],
		    vxarg[5], vxarg[6], vxarg[7], vxarg[8], vxarg[9]));
}

/* Entry point for unix:cmd in post-4.1 erlang - uses "sh -c 'cmd...'" */
int sh(argc, argv)
int argc;
char **argv;
{
    int ll = strlen(argv[argc-1]) - 1;

#ifdef DEBUG
    int i;
    fdprintf(2, "sh:");
    for (i = 1; i < argc; i++)
	fdprintf(2, " %s", argv[i]);
    fdprintf(2, "\n");
#endif
    if (strcmp(argv[1], "-c") != 0 ||
	argv[2][0] != '\'' || argv[argc-1][ll] != '\'')
	return(ERROR);
    argv[argc-1][ll] = '\0';	/* delete trailing ' */
    argv[2]++;			/* skip leading ' (*after* the above!) */
    return(vxcall(argc-1, argv+1));
}

/* Lookup symbol; get address for text symbols, value (assuming int)
   otherwise; return OK or ERROR on failure
   Symbol name is null-terminated and without the leading '_' */
STATUS
lookup(sym, stype, value)
char *sym, **value;
int stype;
{
    char buf[256];
    char *symname = buf;
    int len, ret;
    SYM_TYPE type;

    len = strlen(sym);
    if (len > 254 && (symname = malloc(len+2)) == NULL)
	return(ERROR);
#if defined _ARCH_PPC || defined SIMSPARCSOLARIS
    /* GCC for PPC or SIMSPARC doesn't add a leading _ to symbols */
    strcpy(symname, sym);
#else
    sprintf(symname, "_%s", sym);
#endif
    ret = (stype != 0) ?
	symFindByNameAndType(sysSymTbl, symname, value, &type, stype, stype) :
	    symFindByName(sysSymTbl, symname, value, &type);
    if (symname != buf)
	free(symname);
    if (ret == OK && (type & N_TEXT) == 0) /* get value */
	*value = (char *)*((int *)*value);
    return(ret);
}
