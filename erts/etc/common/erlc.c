/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
 * Purpose: Common compiler front-end.
 */
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#ifdef __WIN32__
#include <winbase.h>
/* FIXE ME config_win32.h? */
#define HAVE_STRERROR 1
#define snprintf _snprintf
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

#ifdef __WIN32__
static int pause_after_execution = 0;
#endif

/*
 * Local functions.
 */

static char* process_opt(int* pArgc, char*** pArgv, int offset);
static void error(char* format, ...);
static void* emalloc(size_t size);
static char* strsave(char* string);
static void push_words(char* src);
static int run_erlang(char* name, char** argv);
static char* get_default_emulator(char* progname);
#ifdef __WIN32__
static char* possibly_quote(char* arg);
static void* erealloc(void *p, size_t size);
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
    char  *value=NULL;
    wchar_t *wcvalue = NULL;
    wchar_t wckey[256];
    int len; 

    MultiByteToWideChar(CP_UTF8, 0, key, -1, wckey, 256);
    
    while (1) {
	DWORD nsz;
	if (wcvalue)
	    free(wcvalue);
	wcvalue = (wchar_t *) emalloc(size*sizeof(wchar_t));
	SetLastError(0);
	nsz = GetEnvironmentVariableW(wckey, wcvalue, size);
	if (nsz == 0 && GetLastError() == ERROR_ENVVAR_NOT_FOUND) {
	    free(wcvalue);
	    return NULL;
	}
	if (nsz <= size) {
	    len = WideCharToMultiByte(CP_UTF8, 0, wcvalue, -1, NULL, 0, NULL, NULL);
	    value = emalloc(len*sizeof(char));
	    WideCharToMultiByte(CP_UTF8, 0, wcvalue, -1, value, len, NULL, NULL);
	    free(wcvalue);
	    return value;
	}
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

#ifdef __WIN32__
int wmain(int argc, wchar_t **wcargv)
{
    char** argv;
#else
int main(int argc, char** argv)
{
#endif
    int eargv_size;
    int eargc_base;		/* How many arguments in the base of eargv. */
    char* emulator;
    char *env;

#ifdef __WIN32__
    int i;
    int len;
    /* Convert argv to utf8 */
    argv = emalloc((argc+1) * sizeof(char*));
    for (i=0; i<argc; i++) {
	len = WideCharToMultiByte(CP_UTF8, 0, wcargv[i], -1, NULL, 0, NULL, NULL);
	argv[i] = emalloc(len*sizeof(char));
	WideCharToMultiByte(CP_UTF8, 0, wcargv[i], -1, argv[i], len, NULL, NULL);
    }
    argv[argc] = NULL;
#endif

    env = get_env("ERLC_EMULATOR");
    emulator = env ? env : get_default_emulator(argv[0]);

    if (strlen(emulator) >= MAXPATHLEN)
        error("Value of environment variable ERLC_EMULATOR is too large");

    /*
     * Allocate the argv vector to be used for arguments to Erlang.
     * Arrange for starting to pushing information in the middle of
     * the array, to allow easy adding of emulator options (like -pa)
     * before '-s erlcompile compile_cmdline...'.
     *
     * Oh, by the way, we will push the compiler command in the
     * base of the eargv vector, and move it up later.
     */

    eargv_size = argc*6+100;
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

    PUSH("+sbtu");
    PUSH("+A0");
    PUSH("-noinput");
    PUSH2("-mode", "minimal");
    PUSH2("-boot", "start_clean");
    PUSH3("-s", "erl_compile", "compile_cmdline");
    PUSH("-extra");

    /*
     * Push standard arguments to Erlang.
     */

    /*
     * Parse all command line switches.
     */

    while (argc > 1) {

	/*
	 * Options starting with '+' are passed on to Erlang.
	 */

	switch (argv[1][0]) {
	case '+':
	    PUSH(argv[1]);
	    break;
	case '-':
	    switch (argv[1][1]) {
	    case 'd':
		if (argv[1][2] == '\0') {
		    debug = 1;
		} else {
		    PUSH(argv[1]);
		}
		break;
	    case 'p':
		{
		    int c = argv[1][2];
		    
		    if (c != 'a' && c != 'z') {
			PUSH(argv[1]);
#ifdef __WIN32__
		    } else if (strcmp(argv[1], "-pause") == 0) {
			pause_after_execution = 1;
#endif
		    } else {
			char option[4];

			UNSHIFT(process_opt(&argc, &argv, 1));
			option[0] = '-';
			option[1] = 'p';
			option[2] = c;
			option[3] = '\0';
			UNSHIFT(strsave(option));
		    }
		}
		break;
	    case 's':
		if (strcmp(argv[1], "-smp") == 0) {
		    UNSHIFT(argv[1]);
		} else {
		    PUSH(argv[1]);
		}
		break;
	    default:
		PUSH(argv[1]);
		break;
	    }
	    break;
	default:
	    PUSH(argv[1]);
	    break;
	}
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

static char*
process_opt(int* pArgc, char*** pArgv, int offset)
{
    int argc = *pArgc;
    char** argv = *pArgv;
    int c = argv[1][1];
    
    if (argv[1][2+offset] != '\0') {
	/*
	 * The option was given as -x<value>.
	 */
	return argv[1]+2+offset;
    }

    /*
     * Look at the next argument.
     */

    argc--, argv++;
    if (argc < 2 || argv[1][0] == '-')
	error("No value given to -%c option", c);
    *pArgc = argc;
    *pArgv = argv;
    return argv[1];
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
wchar_t *make_commandline(char **argv)
{
    static wchar_t *buff = NULL;
    static int siz = 0;
    int num = 0, len;
    char **arg;
    wchar_t *p;

    if (*argv == NULL) { 
	return L"";
    }
    for (arg = argv; *arg != NULL; ++arg) {
	num += strlen(*arg)+1;
    }
    if (!siz) {
	siz = num;
	buff = (wchar_t *) emalloc(siz*sizeof(wchar_t));
    } else if (siz < num) {
	siz = num;
	buff = (wchar_t *) erealloc(buff,siz*sizeof(wchar_t));
    }
    p = buff;
    num=0;
    for (arg = argv; *arg != NULL; ++arg) {
	len = MultiByteToWideChar(CP_UTF8, 0, *arg, -1, p, siz);
	p+=(len-1);
	*p++=L' ';
    }
    *(--p) = L'\0';

    if (debug) {
	printf("Processed command line:%S\n",buff);
    }
    return buff;
}

int my_spawnvp(char **argv)
{
    STARTUPINFOW siStartInfo;
    PROCESS_INFORMATION piProcInfo;
    DWORD ec;

    memset(&siStartInfo,0,sizeof(STARTUPINFOW));
    siStartInfo.cb = sizeof(STARTUPINFOW); 
    siStartInfo.dwFlags = STARTF_USESTDHANDLES;
    siStartInfo.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    siStartInfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    siStartInfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);
    siStartInfo.wShowWindow = SW_HIDE;
    siStartInfo.dwFlags |= STARTF_USESHOWWINDOW;


    if (!CreateProcessW(NULL, 
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
	fprintf(stderr, "erlc: Error executing '%s': %d", progname, 
		GetLastError());
    }
    if (pause_after_execution) {
	fprintf(stderr, "Press ENTER to continue . . .\n");
	while (getchar() != '\n')
	    ;
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
    fprintf(stderr, "erlc: %s\n", sbuf);
    exit(1);
}

static void*
emalloc(size_t size)
{
  void *p = malloc(size);
  if (p == NULL)
    error("Insufficient memory");
  return p;
}

#ifdef __WIN32__
static void *
erealloc(void *p, size_t size)
{
    void *res = realloc(p, size);
    if (res == NULL)
    error("Insufficient memory");
    return res;
}
#endif

static char*
strsave(char* string)
{
  char* p = emalloc(strlen(string)+1);
  strcpy(p, string);
  return p;
}

static int 
file_exists(char *progname) 
{
#ifdef __WIN32__
    wchar_t wcsbuf[MAXPATHLEN];
    MultiByteToWideChar(CP_UTF8, 0, progname, -1, wcsbuf, MAXPATHLEN);
    return (_waccess(wcsbuf, 0) != -1);
#else
    return (access(progname, 1) != -1);
#endif
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
	    if(file_exists(sbuf))
		return strsave(sbuf);
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
