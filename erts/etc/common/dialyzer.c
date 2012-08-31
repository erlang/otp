/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2006-2012. All Rights Reserved.
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
 * Purpose: Dialyzer front-end.
 */
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#ifdef __WIN32__
#include <winbase.h>
#endif

#include <ctype.h>

#define NO 0
#define YES 1

#define ASIZE(a) (sizeof(a)/sizeof(a[0]))

static int debug = 0;		/* Bit flags for debug printouts. */

static char** eargv_base;	/* Base of vector. */
static char** eargv;		/* First argument for erl. */

static int eargc;		/* Number of arguments in eargv. */

#ifdef __WIN32__
#  define QUOTE(s) possibly_quote(s)
#  define IS_DIRSEP(c) ((c) == '/' || (c) == '\\')
#  define ERL_NAME "erl.exe"
#else
#  define QUOTE(s) s
#  define IS_DIRSEP(c) ((c) == '/')
#  define ERL_NAME "erl"
#endif

#define UNSHIFT(s) eargc++, eargv--; eargv[0] = QUOTE(s)
#define PUSH(s) eargv[eargc++] = QUOTE(s)
#define PUSH2(s, t) PUSH(s); PUSH(t)
#define PUSH3(s, t, u) PUSH2(s, t); PUSH(u)

/*
 * Local functions.
 */

static void error(char* format, ...);
static char* emalloc(size_t size);
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
	    free(value);
	value = emalloc(size);
	SetLastError(0);
	nsz = GetEnvironmentVariable((LPCTSTR) key, (LPTSTR) value, size);
	if (nsz == 0 && GetLastError() == ERROR_ENVVAR_NOT_FOUND) {
	    free(value);
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
	free(value);
#endif
}

int
main(int argc, char** argv)
{
    int eargv_size;
    int eargc_base;		/* How many arguments in the base of eargv. */
    char* emulator;
    char *env;
    int i;
    int need_shell = 0;

    env = get_env("DIALYZER_EMULATOR");
    emulator = env ? env : get_default_emulator(argv[0]);

    if (strlen(emulator) >= MAXPATHLEN)
        error("Value of environment variable DIALYZER_EMULATOR is too large");

    /*
     * Allocate the argv vector to be used for arguments to Erlang.
     * Arrange for starting to pushing information in the middle of
     * the array, to allow easy addition of commands in the beginning.
     */

    eargv_size = argc*4+100;
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

    for (i = 1; i < argc; i++) {
       if (strcmp(argv[i], "--wx") == 0) {
           PUSH("-smp"); /* wx currently requires SMP enabled */
           break;
       }
    }

    if (argc > 1 && strcmp(argv[1], "-smp") == 0) {
	PUSH("-smpauto");
	argc--, argv++;
    }

    if (argc > 2 && strcmp(argv[1], "+S") == 0) {
	PUSH3("-smp", "+S", argv[2]);
	argc--, argv++;
	argc--, argv++;
    }

    if (argc > 2 && strcmp(argv[1], "+P") == 0) {
	PUSH2("+P", argv[2]);
	argc--, argv++;
	argc--, argv++;
    } else PUSH2("+P", "1000000");

    if (argc > 2 && strcmp(argv[1], "+sbt") == 0) {
	PUSH2("+sbt", argv[2]);
	argc--, argv++;
	argc--, argv++;
    }

    PUSH("+B");
    PUSH2("-boot", "start_clean");
    PUSH3("-run", "dialyzer", "plain_cl");
    PUSH("-extra");

    /*
     * Push everything except --shell.
     */

    while (argc > 1) {
	if (strcmp(argv[1], "--shell") == 0) {
	    need_shell = 1;
	} else {
	    PUSH(argv[1]);
	}
	argc--, argv++;
    }

    if (!need_shell) {
	UNSHIFT("-noinput");
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
    char sbuf[MAXPATHLEN];
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
	buff = malloc(siz*sizeof(char));
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
	printf("Processed commandline:%s\n",buff);
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
    siStartInfo.wShowWindow = SW_HIDE;
    siStartInfo.dwFlags |= STARTF_USESHOWWINDOW;


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
	fprintf(stderr, "dialyzer: Error executing '%s': %d", progname, 
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
    fprintf(stderr, "dialyzer: %s\n", sbuf);
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
    int mustQuote = NO;
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
	    mustQuote = YES;
	    continue;
	case '"':
	    mustQuote = YES;
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
