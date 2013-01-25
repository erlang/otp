/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2007-2013. All Rights Reserved.
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
 * Purpose: escript front-end.
 */
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#ifdef __WIN32__
#include <winbase.h>
#endif

#include <ctype.h>

static int debug = 0;		/* Bit flags for debug printouts. */

static char** eargv_base;	/* Base of vector. */
static char** eargv;		/* First argument for erl. */

static int eargc;		/* Number of arguments in eargv. */

#define BOOL int
#define FALSE 0
#define TRUE 1

#ifdef __WIN32__
#  define QUOTE(s) possibly_quote(s)
#  define IS_DIRSEP(c) ((c) == '/' || (c) == '\\')
#  define DIRSEPSTR "\\"
#  define PATHSEPSTR ";"
#  define PMAX MAX_PATH
#  define ERL_NAME "erl.exe"
#else
#  define QUOTE(s) s
#  define IS_DIRSEP(c) ((c) == '/')
#  define DIRSEPSTR "/"
#  define PATHSEPSTR ":"
#  define PMAX PATH_MAX
#  define ERL_NAME "erl"
#endif

#define UNSHIFT(s) eargc++, eargv--; eargv[0] = QUOTE(s)
#define UNSHIFT3(s, t, u) UNSHIFT(u); UNSHIFT(t); UNSHIFT(s)
#define PUSH(s) eargv[eargc++] = QUOTE(s)
#define PUSH2(s, t) PUSH(s); PUSH(t)
#define PUSH3(s, t, u) PUSH2(s, t); PUSH(u)
#define LINEBUFSZ 1024

/*
 * Local functions.
 */

static void error(char* format, ...);
static char* emalloc(size_t size);
static void efree(void *p);
static char* strsave(char* string);
static void push_words(char* src);
static int run_erlang(char* name, char** argv);
static char* get_default_emulator(char* progname);
#ifdef __WIN32__
static char* possibly_quote(char* arg);
#endif

/*
 * Supply a strerror() function if libc doesn't.
 */
#ifndef HAVE_STRERROR

extern int sys_nerr;

#ifndef SYS_ERRLIST_DECLARED
extern const char * const sys_errlist[];
#endif /* !SYS_ERRLIST_DECLARED */

char *strerror(int errnum)
{
  static char *emsg[1024];

  if (errnum != 0) {
    if (errnum > 0 && errnum < sys_nerr) 
      sprintf((char *) &emsg[0], "(%s)", sys_errlist[errnum]);
    else 
      sprintf((char *) &emsg[0], "errnum = %d ", errnum);
  }
  else {
    emsg[0] = '\0';
  }
  return (char *) &emsg[0];
}
#endif /* !HAVE_STRERROR */

static char *
get_env(char *key)
{
#ifdef __WIN32__
    DWORD size = 32;
    char *value = NULL;
    while (1) {
	DWORD nsz;
	if (value)
	    efree(value);
	value = emalloc(size);
	SetLastError(0);
	nsz = GetEnvironmentVariable((LPCTSTR) key, (LPTSTR) value, size);
	if (nsz == 0 && GetLastError() == ERROR_ENVVAR_NOT_FOUND) {
	    efree(value);
	    return NULL;
	}
	if (nsz <= size)
	    return value;
	size = nsz;
    }
#else
    return getenv(key);
#endif
}

static void
free_env_val(char *value)
{
#ifdef __WIN32__
    if (value)
	efree(value);
#endif
}
/*
 * Find absolute path to this program
 */

static char *
find_prog(char *origpath)
{
    char relpath[PMAX];
    char abspath[PMAX];

    if (strlen(origpath) >= sizeof(relpath))
        error("Path too long");

    strcpy(relpath, origpath);

    if (strstr(relpath, DIRSEPSTR) == NULL) {
        /* Just a base name */
        char *envpath;

        envpath = get_env("PATH");
        if (envpath) {
            /* Try to find the executable in the path */
            char dir[PMAX];
            char *beg = envpath;
            char *end;
            int sz;

#ifdef __WIN32__
	    HANDLE dir_handle;	        /* Handle to directory. */
	    char wildcard[PMAX];	/* Wildcard to search for. */
	    WIN32_FIND_DATA find_data;	/* Data found by FindFirstFile() or FindNext(). */
#else
            DIR *dp;             /* Pointer to directory structure. */
            struct dirent* dirp; /* Pointer to directory entry.     */
#endif /* __WIN32__ */

            BOOL look_for_sep = TRUE;

            while (look_for_sep) {
                end = strstr(beg, PATHSEPSTR);
                if (end != NULL) {
                    sz = end - beg;
                } else {
                    sz = strlen(beg);
                    look_for_sep = FALSE;
                }
                if (sz >= sizeof(dir)) {
                    beg = end + 1;
                    continue;
                }
                strncpy(dir, beg, sz);
                dir[sz] = '\0';
                beg = end + 1;

#ifdef __WIN32__
		erts_snprintf(wildcard, sizeof(wildcard), "%s" DIRSEPSTR "%s",
            dir, relpath /* basename */);
		dir_handle = FindFirstFile(wildcard, &find_data);
		if (dir_handle == INVALID_HANDLE_VALUE) {
		    /* Try next directory in path */
		    continue;
		} else {
		    /* Wow we found the executable. */
		    strcpy(relpath, wildcard);
		    FindClose(dir_handle);
		    look_for_sep = FALSE;
		    break;
		}
#else
                dp = opendir(dir);
                if (dp != NULL) {
                    while (TRUE) {
                        dirp = readdir(dp);
                        if (dirp == NULL) {
                            closedir(dp);
                            /* Try next directory in path */
                            break;
                        }

                        if (strcmp(origpath, dirp->d_name) == 0) {
                            /* Wow we found the executable. */
                            erts_snprintf(relpath, sizeof(relpath), "%s" DIRSEPSTR "%s",
                                dir, dirp->d_name);
                            closedir(dp);
			    look_for_sep = FALSE;
                            break;
                        }
                    }
                }
#endif /* __WIN32__ */
            }
        }
    }
    
    {
#ifdef __WIN32__
	DWORD size;
	char *absrest;
	size = GetFullPathName(relpath, PMAX, abspath, &absrest);
	if ((size == 0) || (size > PMAX)) {
	
#else
        if (!realpath(relpath, abspath)) {
#endif /* __WIN32__ */
	    /* Cannot determine absolute path to escript. Try the origin.  */
	    return strsave(origpath);
	} else {
	    return strsave(abspath);
	}
    }
}

static void
append_shebang_args(char* scriptname)
{
    /* Open script file */
    FILE* fd = fopen (scriptname,"r");

    if (fd != NULL)	{
	/* Read first line in script file */
	static char linebuf[LINEBUFSZ];
	char* ptr = fgets(linebuf, LINEBUFSZ, fd);

	if (ptr != NULL) {
	    /* Try to find args on second or third line */
	    ptr = fgets(linebuf, LINEBUFSZ, fd);
	    if (ptr != NULL && linebuf[0] == '%' && linebuf[1] == '%' && linebuf[2] == '!') {
		/* Use second line */
	    } else {
		/* Try third line */
		ptr = fgets(linebuf, LINEBUFSZ, fd);
		if (ptr != NULL && linebuf[0] == '%' && linebuf[1] == '%' && linebuf[2] == '!') {
		    /* Use third line */
		} else {
		    /* Do not use any line */
		    ptr = NULL;
		}
	    }
	  
	    if (ptr != NULL) {
		/* Use entire line but the leading chars */
		char* beg = linebuf + 3;
		char* end;
		BOOL newline = FALSE;
		
		/* Push all args */
		while(beg && !newline) {
		    /* Skip leading spaces */
		    while (beg && beg[0] == ' ') {
			beg++;
		    }
		    
		    /* Find end of arg */
		    end = beg;
		    while (end && end < (linebuf+LINEBUFSZ-1) && end[0] != ' ') {
			if (end[0] == '\n') {
			    newline = TRUE;
			    end[0]= '\0';
			    break;
			} else {
			    end++;
			}
		    }

		    /* Empty arg */
		    if (beg == end) {
			break;
		    }
		    end[0]= '\0';
		    PUSH(beg);
		    beg = end + 1;
		}
	    } 
	}
	fclose(fd);
    } else {
	error("Failed to open file: %s", scriptname);
    }
}

int
main(int argc, char** argv)
{
    int eargv_size;
    int eargc_base;		/* How many arguments in the base of eargv. */
    char* emulator;
    char* env;
    char* basename;
    char* absname;
    char scriptname[PMAX];
    char** last_opt;
    char** first_opt;

    emulator = env = get_env("ESCRIPT_EMULATOR");
    if (emulator == NULL) {
	emulator = get_default_emulator(argv[0]);
    }

    if (strlen(emulator) >= PMAX)
        error("Value of environment variable ESCRIPT_EMULATOR is too large");

    /*
     * Allocate the argv vector to be used for arguments to Erlang.
     * Arrange for starting to pushing information in the middle of
     * the array, to allow easy addition of commands in the beginning.
     */

    eargv_size = argc*4+1000+LINEBUFSZ/2;
    eargv_base = (char **) emalloc(eargv_size*sizeof(char*));
    eargv = eargv_base;
    eargc = 0;
    push_words(emulator);
    eargc_base = eargc;
    eargv = eargv + eargv_size/2;
    eargc = 0;

    free_env_val(env);

    /*
     * Push initial arguments.
     */

    PUSH("+B");
    PUSH2("-boot", "start_clean");
    PUSH("-noshell");

    /* Determine basename of the executable */
    for (basename = argv[0]+strlen(argv[0]);
	 basename > argv[0] && !(IS_DIRSEP(basename[-1]));
	 --basename)
	;
    
    first_opt = argv;
    last_opt = argv;

#ifdef __WIN32__
    if ( (_stricmp(basename, "escript.exe") == 0)
       ||(_stricmp(basename, "escript") == 0)) {
#else
    if (strcmp(basename, "escript") == 0) {
#endif
	/*
	 * Locate all options before the script name.
	 */
	
	while (argc > 1 && argv[1][0] == '-') {
	    argc--;
	    argv++;
	    last_opt = argv;
	}
	
	if (argc <= 1) {
	    error("Missing filename\n");
	}
	strncpy(scriptname, argv[1], sizeof(scriptname));
	scriptname[sizeof(scriptname)-1] = '\0';
	argc--;
	argv++;
    } else {
#ifdef __WIN32__
	int len;
#endif
	absname = find_prog(argv[0]);
#ifdef __WIN32__
	len = strlen(absname);
	if (len >= 4 && _stricmp(absname+len-4, ".exe") == 0) {
	    absname[len-4] = '\0';
	}
#endif

	erts_snprintf(scriptname, sizeof(scriptname), "%s.escript",
        absname);
	efree(absname);

    }

    /*
     * Read options from the %%! row in the script and add them as args
     */

    append_shebang_args(scriptname);

    /*
     * Push the script name and everything following it as extra arguments.
     */

    PUSH3("-run", "escript", "start");

    /*
     * Push all options before the script name. But omit the leading hyphens.
     */
    
    while (first_opt != last_opt) {
	PUSH(first_opt[1]+1);
	first_opt++;
    }

    PUSH("-extra");
    PUSH(scriptname);
    while (argc > 1) {
	PUSH(argv[1]);
	argc--, argv++;
    }

    /*
     * Move up the commands for invoking the emulator and adjust eargv
     * accordingly.
     */

    while (--eargc_base >= 0) {
	UNSHIFT(eargv_base[eargc_base]);
    }
    
    /*
     * Invoke Erlang with the collected options.
     */

    PUSH(NULL);
    return run_erlang(eargv[0], eargv);
}

static void
push_words(char* src)
{
    char sbuf[PMAX];
    char* dst;

    dst = sbuf;
    while ((*dst++ = *src++) != '\0') {
	if (isspace((int)*src)) {
	    *dst = '\0';
	    PUSH(strsave(sbuf));
	    dst = sbuf;
	    do {
		src++;
	    } while (isspace((int)*src));
	}
    }
    if (sbuf[0])
	PUSH(strsave(sbuf));
}
#ifdef __WIN32__
char *make_commandline(char **argv)
{
    static char *buff = NULL;
    static int siz = 0;
    int num = 0;
    char **arg, *p;

    if (*argv == NULL) { 
	return "";
    }
    for (arg = argv; *arg != NULL; ++arg) {
	num += strlen(*arg)+1;
    }
    if (!siz) {
	siz = num;
	buff = emalloc(siz*sizeof(char));
    } else if (siz < num) {
	siz = num;
	buff = realloc(buff,siz*sizeof(char));
    }
    p = buff;
    for (arg = argv; *arg != NULL; ++arg) {
	strcpy(p,*arg);
	p+=strlen(*arg);
	*p++=' ';
    }
    *(--p) = '\0';

    if (debug) {
	printf("Processed command line:%s\n",buff);
    }
    return buff;
}

int my_spawnvp(char **argv)
{
    STARTUPINFO siStartInfo;
    PROCESS_INFORMATION piProcInfo;
    DWORD ec;

    memset(&siStartInfo,0,sizeof(STARTUPINFO));
    siStartInfo.cb = sizeof(STARTUPINFO); 
    siStartInfo.dwFlags = STARTF_USESTDHANDLES;
    siStartInfo.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    siStartInfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    siStartInfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);

    if (!CreateProcess(NULL, 
		       make_commandline(argv),
		       NULL, 
		       NULL, 
		       TRUE, 
		       0,
		       NULL, 
		       NULL, 
		       &siStartInfo, 
		       &piProcInfo)) {
	return -1;
    }
    CloseHandle(piProcInfo.hThread);

    WaitForSingleObject(piProcInfo.hProcess,INFINITE);
    if (!GetExitCodeProcess(piProcInfo.hProcess,&ec)) {
	return 0;
    }
    return (int) ec;
}    
#endif /* __WIN32__ */


static int
run_erlang(char* progname, char** argv)
{
#ifdef __WIN32__
    int status;
#endif

    if (debug) {
	int i = 0;
	while (argv[i] != NULL)
	    printf(" %s", argv[i++]);
	printf("\n");
    }

#ifdef __WIN32__
    /*
     * Alas, we must wait here for the program to finish.
     * Otherwise, the shell from which we was executed will think
     * we are finished and print a prompt and read keyboard input.
     */

    status = my_spawnvp(argv)/*_spawnvp(_P_WAIT,progname,argv)*/;
    if (status == -1) {
	fprintf(stderr, "escript: Error executing '%s': %d", progname, 
		GetLastError());
    }
    return status;
#else
    execvp(progname, argv);
    error("Error %d executing \'%s\'.", errno, progname);
    return 2;
#endif
}

static void
error(char* format, ...)
{
    char sbuf[1024];
    va_list ap;
    
    va_start(ap, format);
    erts_vsnprintf(sbuf, sizeof(sbuf), format, ap);
    va_end(ap);
    fprintf(stderr, "escript: %s\n", sbuf);
    exit(1);
}

static char*
emalloc(size_t size)
{
  char *p = malloc(size);
  if (p == NULL)
    error("Insufficient memory");
  return p;
}

static void
efree(void *p) 
{
    free(p);
}

static char*
strsave(char* string)
{
    char* p = emalloc(strlen(string)+1);
    strcpy(p, string);
    return p;
}

static char*
get_default_emulator(char* progname)
{
    char sbuf[MAXPATHLEN];
    char* s;

    if (strlen(progname) >= sizeof(sbuf))
        return ERL_NAME;

    strcpy(sbuf, progname);
    for (s = sbuf+strlen(sbuf); s >= sbuf; s--) {
	if (IS_DIRSEP(*s)) {
	    strcpy(s+1, ERL_NAME);
#ifdef __WIN32__
	    if (_access(sbuf, 0) != -1) {
		return strsave(sbuf);
	    }
#else
	    if (access(sbuf, 1) != -1) {
		return strsave(sbuf);
	    }
#endif
	    break;
	}
    }
    return ERL_NAME;
}

#ifdef __WIN32__
static char*
possibly_quote(char* arg)
{
    int mustQuote = FALSE;
    int n = 0;
    char* s;
    char* narg;

    if (arg == NULL) {
	return arg;
    }

    /*
     * Scan the string to find out if it needs quoting and return
     * the original argument if not.
     */

    for (s = arg; *s; s++, n++) {
	switch(*s) {
	case ' ':
	    mustQuote = TRUE;
	    continue;
	case '"':
	    mustQuote = TRUE;
	    n++;
	    continue;
	case '\\':
	    if(s[1] == '"')
		n++;
	    continue;
	default:
	    continue;
	}
    }
    if (!mustQuote) {
	return arg;
    }

    /*
     * Insert the quotes and put a backslash in front of every quote
     * inside the string.
     */

    s = narg = emalloc(n+2+1);
    for (*s++ = '"'; *arg; arg++, s++) {
	if (*arg == '"' || (*arg == '\\' && arg[1] == '"')) {
	    *s++ = '\\';
	}
	*s = *arg;
    }
    if (s[-1] == '\\') {
	*s++ ='\\';
    }
    *s++ = '"';
    *s = '\0';
    return narg;
}
#endif /* __WIN32__ */
