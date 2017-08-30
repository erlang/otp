/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
 * Purpose: Common Test front-end.
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
#define PUSH4(s, t, u, v) PUSH2(s, t); PUSH2(u, v)

/*
 * The possible modes to start Common Test
 */

#define NORMAL_MODE 0
#define VTS_MODE 1
#define CT_SHELL_MODE 2
#define MASTER_MODE 3
#define ERL_SHELL_MODE 4

/*
 * Distribution
 */

#define SHORT_NAME 0
#define FULL_NAME 1

/*
 * Local functions.
 */

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
    char nodename[100];
    char browser[100];
    int ct_mode;
    int dist_mode;
    int cnt;
    int erl_args;
    char** argv0;

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
    argv0 = argv;

    emulator = get_default_emulator(argv[0]);

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

    strcpy(nodename, "ct");
    dist_mode = SHORT_NAME;
    browser[0] = '\0';
    ct_mode = NORMAL_MODE;
    erl_args = argc;
    cnt = 1;

    /*
     * Check various flags before building command line
     */

    while (cnt < argc) {
	if (strcmp(argv[1], "-erl_args") == 0) {
	    erl_args = cnt;
	}
	else if (strcmp(argv[1], "-sname") == 0) {
	    strncpy(nodename, argv[2], sizeof(nodename));
	    nodename[sizeof(nodename)-1] = '\0';
	    cnt++, argv++;
	}
	else if (strcmp(argv[1], "-name") == 0) {
	    strncpy(nodename, argv[2], sizeof(nodename));
	    nodename[sizeof(nodename)-1] = '\0';
	    dist_mode = FULL_NAME;
	    cnt++, argv++;
	}
	else {
	    if (cnt < erl_args) {
		if (strcmp(argv[1], "-vts") == 0) {
		    ct_mode = VTS_MODE;
		}
		else if (strcmp(argv[1], "-browser") == 0) {
		    strncpy(browser, argv[2], sizeof(browser));
		    browser[sizeof(browser)-1] = '\0';
		    cnt++, argv++;
		}
		else if (strcmp(argv[1], "-shell") == 0) {
		    ct_mode = CT_SHELL_MODE;
		}
		else if (strcmp(argv[1], "-ctmaster") == 0) {
		    strcpy(nodename, "ct_master");
		    ct_mode = MASTER_MODE;
		}
		else if (strcmp(argv[1], "-ctname") == 0) {
		    strncpy(nodename, argv[2], sizeof(nodename));
		    nodename[sizeof(nodename)-1] = '\0';
		    ct_mode = ERL_SHELL_MODE;
		    cnt++, argv++;
		}
	    }
	}
	cnt++, argv++;
    }

    argv = argv0;

    /*
     * Push initial arguments.
     */

    if (dist_mode == FULL_NAME) {
	PUSH2("-name", nodename);
    }
    else {
	PUSH2("-sname", nodename);
    }

    /*
     * Push everything else
     */

    if (ct_mode == VTS_MODE) {
	PUSH4("-s", "ct_webtool", "script_start", "vts");
	if (browser[0] != '\0') PUSH(browser);
	PUSH3("-s", "ct_run", "script_start");
    }
    else if (ct_mode == CT_SHELL_MODE) {
	PUSH3("-s", "ct_run", "script_start");
    }
    else if (ct_mode == NORMAL_MODE) {
	PUSH3("-s", "ct_run", "script_start");
	PUSH3("-s", "erlang", "halt");
    }

    cnt = 1;
    while (cnt < argc) {
	if (strcmp(argv[1], "-erl_args") == 0) {
	    PUSH("-ct_erl_args");
	}
	else if ((strcmp(argv[1], "-sname") == 0) || (strcmp(argv[1], "-name") == 0)) {
	    cnt++, argv++;
	}
	else if (cnt < erl_args) {
	    if (strcmp(argv[1], "-config") == 0)
		PUSH("-ct_config");
	    else if (strcmp(argv[1], "-decrypt_key") == 0)
		PUSH("-ct_decrypt_key");
	    else if (strcmp(argv[1], "-decrypt_file") == 0)
		PUSH("-ct_decrypt_file");
	    else
		PUSH(argv[1]);
	}
	else {
	    PUSH(argv[1]);
	}
	cnt++, argv++;
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
     * Otherwise, the shell from which we were executed will think
     * we are finished and print a prompt and read keyboard input.
     */

    status = my_spawnvp(argv)/*_spawnvp(_P_WAIT,progname,argv)*/;
    if (status == -1) {
	fprintf(stderr, "ct_run: Error executing '%s': %d", progname,
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
    fprintf(stderr, "ct_run: %s\n", sbuf);
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
