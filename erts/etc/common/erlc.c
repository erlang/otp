/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1997-2010. All Rights Reserved.
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

static char* output_type = NULL; /* Type of output file. */
#ifdef __WIN32__
static int pause_after_execution = 0;
#endif

/*
 * Local functions.
 */

static char* process_opt(int* pArgc, char*** pArgv, int offset);
static void error(char* format, ...);
static void usage(void);
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
    char cwd[MAXPATHLEN];	/* Current working directory. */
    int eargv_size;
    int eargc_base;		/* How many arguments in the base of eargv. */
    char* emulator;
    char *env;

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

    PUSH("-noinput");
    PUSH2("-mode", "minimal");
    PUSH2("-boot", "start_clean");
    PUSH3("-s", "erl_compile", "compile_cmdline");

    /*
     * Push standard arguments to Erlang.
     *
     * The @cwd argument was once needed, but from on R13B02 is optional.
     * For maximum compatibility between erlc and erl of different versions,
     * still provide the @cwd argument, unless it is too long to be
     * represented as an atom.
     */
    if (getcwd(cwd, sizeof(cwd)) == NULL)
	error("Failed to get current working directory: %s", strerror(errno));
#ifdef __WIN32__
    (void) GetShortPathName(cwd, cwd, sizeof(cwd));
#endif    
    if (strlen(cwd) < 256) {
	PUSH2("@cwd", cwd);
    }

    /*
     * Parse all command line switches.
     */

    while (argc > 1 && (argv[1][0] == '-' || argv[1][0] == '+')) {

	/*
	 * Options starting with '+' are passed on to Erlang.
	 */

	if (argv[1][0] == '+') {
	    PUSH2("@option", argv[1]+1);
	} else {
	    /*
	     * Interpret options starting with '-'.
	     */
	    
	    switch (argv[1][1]) {
	    case 'b':
		output_type = process_opt(&argc, &argv, 0);
		PUSH2("@output_type", output_type);
		break;
	    case 'c':		/* Allowed for compatibility with 'erl'. */
		if (strcmp(argv[1], "-compile") != 0)
		    goto error;
		break;
	    case 'd':
		debug = 1;
		break;
	    case 'D':
		{
		    char* def = process_opt(&argc, &argv, 0);
		    char* equals;
		    
		    def = strsave(def);	/* Do not clobber original. */
		    if ((equals = strchr(def, '=')) == NULL) {
			PUSH2("@d", def);
		    } else {
			*equals = '\0';
			equals++;
			PUSH3("@dv", def, equals);
		    }
		}
		break;
	    case 'h':
		if (strcmp(argv[1], "-hybrid") == 0) {
		    UNSHIFT(argv[1]);
		} else {
		    usage();
		}
		break;
	    case 'I':
		PUSH2("@i", process_opt(&argc, &argv, 0));
		break;
	    case 'M':
		{
		    char *buf, *key, *val;
		    size_t buf_len;

		    if (argv[1][2] == '\0') { /* -M */
			/* Push the following options:
			 *   o  'makedep'
			 *   o  {makedep_output, standard_io}
			 */
			buf = strsave("makedep");
			PUSH2("@option", buf);

			key = "makedep_output";
			val = "standard_io";
			buf_len = 1 + strlen(key) + 1 + strlen(val) + 1 + 1;
			buf = emalloc(buf_len);
			snprintf(buf, buf_len, "{%s,%s}", key, val);
			PUSH2("@option", buf);
		    } else if (argv[1][3] == '\0') {
			switch(argv[1][2]) {
			case 'D': /* -MD */
			    /* Push the following options:
			     *   o  'makedep'
			     */
			    buf = strsave("makedep");
			    PUSH2("@option", buf);
			    break;
			case 'F': /* -MF <file> */
			    /* Push the following options:
			     *   o  'makedep'
			     *   o  {makedep_output, <file>}
			     */
			    buf = strsave("makedep");
			    PUSH2("@option", buf);

			    key = "makedep_output";
			    val = process_opt(&argc, &argv, 1);
			    buf_len = 1 + strlen(key) + 2 + strlen(val) + 2 + 1;
			    buf = emalloc(buf_len);
			    snprintf(buf, buf_len, "{%s,\"%s\"}", key, val);
			    PUSH2("@option", buf);
			    break;
			case 'T': /* -MT <target> */
			    /* Push the following options:
			     *   o  {makedep_target, <target>}
			     */
			    key = "makedep_target";
			    val = process_opt(&argc, &argv, 1);
			    buf_len = 1 + strlen(key) + 2 + strlen(val) + 2 + 1;
			    buf = emalloc(buf_len);
			    snprintf(buf, buf_len, "{%s,\"%s\"}", key, val);
			    PUSH2("@option", buf);
			    break;
			case 'Q': /* -MQ <target> */
			    /* Push the following options:
			     *   o  {makedep_target, <target>}
			     *   o  makedep_quote_target
			     */
			    key = "makedep_target";
			    val = process_opt(&argc, &argv, 1);
			    buf_len = 1 + strlen(key) + 2 + strlen(val) + 2 + 1;
			    buf = emalloc(buf_len);
			    snprintf(buf, buf_len, "{%s,\"%s\"}", key, val);
			    PUSH2("@option", buf);

			    buf = strsave("makedep_quote_target");
			    PUSH2("@option", buf);
			    break;
			case 'G': /* -MG */
			    /* Push the following options:
			     *   o  makedep_add_missing
			     */
			    buf = strsave("makedep_add_missing");
			    PUSH2("@option", buf);
			    break;
			case 'P': /* -MP */
			    /* Push the following options:
			     *   o  makedep_phony
			     */
			    buf = strsave("makedep_add_missing");
			    PUSH2("@option", buf);
			    break;
			default:
			    goto error;
			}
		    }
		}
		break;
	    case 'o':
		PUSH2("@outdir", process_opt(&argc, &argv, 0));
		break;
	    case 'O':
		PUSH("@optimize");
		if (argv[1][2] == '\0')
		    PUSH("1");
		else
		    PUSH(argv[1]+2);
		break;
	    case 'p':
		{
		    int c = argv[1][2];
		    
		    if (c != 'a' && c != 'z') {
			goto error;
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
		    goto error;
		}
		break;
	    case 'v':		/* Verbose. */
		PUSH2("@verbose", "true");
		break;
	    case 'V':
		/** XXX Version perhaps, but of what? **/
		break;
	    case 'W':		/* Enable warnings. */
		if (strcmp(argv[1]+2, "all") == 0) {
		    PUSH2("@warn", "999");
		} else if (strcmp(argv[1]+2, "error") == 0) {
		    PUSH2("@option", "warnings_as_errors");
		} else if (isdigit((int)argv[1][2])) {
		    PUSH2("@warn", argv[1]+2);
		} else {
		    PUSH2("@warn", "1");
		}
		break;
	    case 'E':
	    case 'S':
	    case 'P':
		{
		    char* buf;

		    /* 
		     * From the given upper-case letter, construct
		     * a quoted atom.  This is a convenience for the
		     * Erlang compiler, to avoid fighting with the shell's
		     * quoting.
		     */

		    buf = emalloc(4);
		    buf[0] = '\'';
		    buf[1] = argv[1][1];
		    buf[2] = '\'';
		    buf[3] = '\0';

		    PUSH2("@option", buf);
		}
		break;

	    case '-':
		goto no_more_options;

	    default:
	    error:
		usage();
		break;
	    }
	}
	argc--, argv++;
    }

 no_more_options:

    if (argc <= 1) {
	/*
	 * To avoid starting an Erlang system unless absolutely needed
	 * exit if no files were specified on the command line.
	 */
	exit(0);
    }

    /*
     * The rest of the command line must be filenames.  Simply push them.
     */

    PUSH("@files");
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
usage(void)
{
    static struct {
	char* name;
	char* desc;
    } options[] = {
	{"-b type", "type of output file (e.g. jam or beam)"},
	{"-d", "turn on debugging of erlc itself"},
	{"-Dname", "define name"},
	{"-Dname=value", "define name to have value"},
	{"-hybrid", "compile using hybrid-heap emulator"},
	{"-help", "shows this help text"},
	{"-I path", "where to search for include files"},
	{"-M", "generate a rule for make(1) describing the dependencies"},
	{"-MF file", "write the dependencies to 'file'"},
	{"-MT target", "change the target of the rule emitted by dependency "
		"generation"},
	{"-MQ target", "same as -MT but quote characters special to make(1)"},
	{"-MG", "consider missing headers as generated files and add them to "
		"the dependencies"},
	{"-MP", "add a phony target for each dependency"},
	{"-MD", "same as -M -MT file (with default 'file')"},
	{"-o name", "name output directory or file"},
	{"-pa path", "add path to the front of Erlang's code path"},
	{"-pz path", "add path to the end of Erlang's code path"},
	{"-smp", "compile using SMP emulator"},
	{"-v", "verbose compiler output"},
	{"-Werror", "make all warnings into errors"},
	{"-W0", "disable warnings"},
	{"-Wnumber", "set warning level to number"},
	{"-Wall", "enable all warnings"},
	{"-W", "enable warnings (default; same as -W1)"},
	{"-E", "generate listing of expanded code (Erlang compiler)"},
	{"-S", "generate assembly listing (Erlang compiler)"},
	{"-P", "generate listing of preprocessed code (Erlang compiler)"},
	{"+term", "pass the Erlang term unchanged to the compiler"},
    };
    int i;

    fprintf(stderr, "Usage:\terlc [options] file.ext ...\n");
    fprintf(stderr, "Options:\n");
    for (i = 0; i < sizeof(options)/sizeof(options[0]); i++) {
	fprintf(stderr, "%-14s %s\n", options[i].name, options[i].desc);
    }
    exit(1);
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
