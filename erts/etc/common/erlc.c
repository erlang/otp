/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1997-2017. All Rights Reserved.
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

#ifdef __WIN32__
#  include <winsock2.h>
#else
#  include <stdio.h>
#  include <limits.h>
#  include <stdlib.h>
#  include <signal.h>
#  include <sys/stat.h>
#endif

#include "etc_common.h"
#include "ei.h"

#define NO 0
#define YES 1

#define ASIZE(a) (sizeof(a)/sizeof(a[0]))

#ifndef PATH_MAX
# define PATH_MAX 4096
#endif

static int debug = 0;		/* Debug level. */
static int use_server = 0;      /* Use compile server. */
static char* source_file = "<no source>"; /* Source file (last argument). */

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

static void get_env_compile_server(void);
static char* process_opt(int* pArgc, char*** pArgv, int offset);
static void error(char* format, ...);
static void* emalloc(size_t size);
#ifdef HAVE_COPYING_PUTENV
static void efree(void *p);
#endif
static char* strsave(char* string);
static void push_words(char* src);
static int run_erlang(char* name, char** argv);
static void call_compile_server(char** argv);
static void encode_env(ei_x_buff* buf);
#ifndef __WIN32__
static char* find_executable(char* progname);
static char* safe_realpath(char* file);
#endif
static char* get_encoding(void);
static char* decode_binary(const char* buf, int* dec_index, int* dec_size);
static void start_compile_server(char* node_name, char** argv);
static char* get_default_emulator(char* progname);
static char* possibly_unquote(char* arg);
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
set_env(char *key, char *value)
{
#ifdef __WIN32__
    WCHAR wkey[MAXPATHLEN];
    WCHAR wvalue[MAXPATHLEN];
    MultiByteToWideChar(CP_UTF8, 0, key, -1, wkey, MAXPATHLEN);
    MultiByteToWideChar(CP_UTF8, 0, value, -1, wvalue, MAXPATHLEN);
    if (!SetEnvironmentVariableW(wkey, wvalue))
        error("SetEnvironmentVariable(\"%s\", \"%s\") failed!", key, value);
#else
    size_t size = strlen(key) + 1 + strlen(value) + 1;
    char *str = emalloc(size);
    sprintf(str, "%s=%s", key, value);
    if (putenv(str) != 0)
        error("putenv(\"%s\") failed!", str);
#ifdef HAVE_COPYING_PUTENV
    efree(str);
#endif
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

    get_env_compile_server();

    ei_init();

    env = get_env("ERLC_EMULATOR");
    emulator = env ? env : get_default_emulator(argv[0]);

    if (strlen(emulator) >= MAXPATHLEN)
        error("Value of environment variable ERLC_EMULATOR is too large");

    /*
     * Add scriptname to env
     */

    set_env("ESCRIPT_NAME", argv[0]);

    /*
     * Save a piece of configuration in an environment variable.  The
     * point is that the compile server needs to know that the same
     * Erlang/OTP system would be started. On Unix, we save the full
     * path to the Erlang emulator. On Windows, we save the value of
     * the environment variable PATH. If the compile server finds that
     * another Erlang/OTP system would be started, it will terminate
     * itself.
     */

#ifdef __WIN32__
    set_env("ERLC_CONFIGURATION", get_env("PATH"));
#else
    {
        char* full_path_emulator = find_executable(emulator);
        set_env("ERLC_CONFIGURATION", full_path_emulator);
    }
#endif

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
    PUSH2("-boot", "no_dot_erlang");
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

        source_file = "<no source>";
	switch (argv[1][0]) {
	case '+':
	    PUSH(argv[1]);
	    break;
	case '-':
	    switch (argv[1][1]) {
	    case 'd':
		if (argv[1][2] == '\0') {
		    debug++;
		} else {
		    PUSH(argv[1]);
		}
		break;
            case 'n':
                if (strcmp(argv[1], "-no-server") == 0) {
                    use_server = 0;
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
                } else if (strcmp(argv[1], "-server") == 0) {
                    use_server = 1;
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
            source_file = argv[1];
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
    if (use_server) {
        call_compile_server(eargv);
    }
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
get_env_compile_server(void)
{
    char* us = get_env("ERLC_USE_SERVER");

    if (us == NULL) {
        return;                 /* Keep default */
    }

    switch (us[0]) {
    case 'f':
        if (strcmp(us+1, "alse") == 0) {
            use_server = 0;
            return;
        }
        break;
    case 'n':
        if (strcmp(us+1, "o") == 0) {
            use_server = 0;
            return;
        }
        break;
    case 't':
        if (strcmp(us+1, "rue") == 0) {
            use_server = 1;
            return;
        }
        break;
    case 'y':
        if (strcmp(us+1, "es") == 0) {
            use_server = 1;
            return;
        }
        break;
    }
    fprintf(stderr, "erlc: Warning: Ignoring unrecognized value '%s' "
            "for environment value ERLC_USE_SERVER\n", us);
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
    wchar_t *buff = NULL;
    int siz = 0;
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

    if (debug > 1) {
	fprintf(stderr, "Processed command line: %S\n", buff);
    }
    return buff;
}

int my_spawnvp(int wait, char **argv)
{
    STARTUPINFOW siStartInfo;
    PROCESS_INFORMATION piProcInfo;
    DWORD ec = 0;

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

    if (wait) {
        WaitForSingleObject(piProcInfo.hProcess,INFINITE);
        if (!GetExitCodeProcess(piProcInfo.hProcess,&ec)) {
            return 0;
        }
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

    if (debug > 0) {
        fprintf(stderr, "spawning erl for %s", source_file);
    }
    if (debug > 1) {
	int i = 0;
        fprintf(stderr, ":\n  ");
	while (argv[i] != NULL) {
	    fprintf(stderr, "%s ", argv[i++]);
        }
    }
    if (debug) {
        putc('\n', stderr);
    }

#ifdef __WIN32__
    /*
     * Alas, we must wait here for the program to finish.
     * Otherwise, the shell from which we was executed will think
     * we are finished and print a prompt and read keyboard input.
     */

    status = my_spawnvp(1, argv);
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
call_compile_server(char** argv)
{
    ei_cnode ec;
    char* user;
    char* server_id;
    char node_name[MAXNODELEN+1];
    char remote[MAXNODELEN+1];
    short creation = 1;
    int fd;
    char cwd[MAXPATHLEN+1];
    ei_x_buff args;
    ei_x_buff reply;
    int reply_size;
    int dec_size, dec_index;
    char atom[MAXATOMLEN];
    int argc;

#ifdef __WIN32__
    if (_getcwd(cwd, sizeof(cwd)) == 0) {
        fprintf(stderr, "erlc: failed to get current working directory\n");
        exit(2);
    }
#else
    if (getcwd(cwd, sizeof(cwd)) == 0) {
        fprintf(stderr, "erlc: failed to get current working directory\n");
        exit(2);
    }
#endif

#ifndef __WIN32__
    {
        struct sigaction act;

        /*
         * If the node is terminating when ei_rpc() is executed, the process
         * may receive a SIGPIPE signal. Make sure it does not kill this process.
         */
        act.sa_handler = SIG_IGN;
        sigemptyset(&act.sa_mask);
        act.sa_flags = 0;
        sigaction(SIGPIPE, &act, NULL);
    }
#endif

    /* Get user name */
    user = get_env("USERNAME");  /* Windows */
    if (!user) {
        user = get_env("LOGNAME"); /* Unix */
    }
    if (!user) {
        user = get_env("USER");  /* Unix */
    }
    if (!user) {
        user = "nouser";
    }

    /* Get build server id, if any */
    server_id = get_env("ERLC_SERVER_ID");
    if (server_id) {
        /* Filter out obviously wrong characters. Making it waterproof would be
         * too complicated and letting an invalid name through won't have any
         * negative consequences beyond bad performance. */
        if (strpbrk(server_id, ",@.!#$%^&=+*/()[]{}|'\"`~?")) {
            error("ERLC_SERVER_ID must not contain characters that are "
                  "invalid in node names.\n");
        }
    } else {
        server_id = "";
    }

    /* Create my own node name. */
#ifdef __WIN32__
    sprintf(node_name, "erlc_client_%s_%lu", user, (unsigned long) GetCurrentProcessId());
#else
    sprintf(node_name, "erlc_client_%s_%d", user, getpid());
#endif

    if (ei_connect_init(&ec, node_name, "erlc_compile_server_cookie", creation) < 0) {
        /*
         * There is probably no .erlang.cookie file.
         */
        if (debug > 1) {
            fprintf(stderr, "\ncan't create C node %s: %s\n",
                    node_name, strerror(erl_errno));
        }
        if(snprintf(remote, MAXNODELEN, "erl_compile_server_%s_%s@host",
                    server_id, user) >= MAXNODELEN) {
            error("Compile server node name is too long.\n"
                  "\tERL_SERVER_ID = %s\n"
                  "\tuser name = %s\n",
                  server_id, user);
        }
        goto start_compile_server;
    }

    /* Create node name for compile server. */

    if(snprintf(remote, MAXNODELEN, "erl_compile_server_%s_%s@%s",
                server_id, user, ei_thishostname(&ec)) >= MAXNODELEN) {
        error("Compile server node name is too long.\n"
              "\tERL_SERVER_ID = %s\n"
              "\tuser name = %s\n",
              server_id, user);
    }

    if ((fd = ei_connect(&ec, remote)) < 0) {
        if (debug > 1) {
            fprintf(stderr, "failed to connect to compile server %s: %s\n",
                    remote, strerror(erl_errno));
        }
        goto start_compile_server;
    }

    /*
     * Encode the request to the compile server.
     */

    ei_x_new_with_version(&args);
    ei_x_encode_list_header(&args, 1);
    ei_x_encode_map_header(&args, 4);
    ei_x_encode_atom(&args, "encoding");
    ei_x_encode_atom(&args, get_encoding());
    ei_x_encode_atom(&args, "cwd");
    ei_x_encode_string(&args, cwd);
    ei_x_encode_atom(&args, "env");
    encode_env(&args);
    ei_x_encode_atom(&args, "command_line");
    argc = 0;
    while (argv[argc]) {
        ei_x_encode_list_header(&args, 1);
        ei_x_encode_string(&args, possibly_unquote(argv[argc]));
        argc++;
    }
    ei_x_encode_empty_list(&args); /* End of command_line */
    ei_x_encode_empty_list(&args); /* End of argument list for apply */

    /*
     * Do a RPC to the compile server.
     */

    ei_x_new_with_version(&reply);
    reply_size = ei_rpc(&ec, fd, "erl_compile_server", "compile",
                        args.buff+1, args.index-1, &reply);
    if (reply_size < 0) {
        if (debug > 1) {
            fprintf(stderr, "failed to rpc to node %s: %s\n",
                    remote, strerror(erl_errno));
        }
        goto start_compile_server;
    }

    /*
     * Decode the answer.
     */

    dec_index = 0;
    if (ei_decode_atom(reply.buff, &dec_index, atom) == 0 &&
        strcmp(atom, "wrong_config") == 0) {
        if (debug > 1) {
            fprintf(stderr, "wrong configuration\n");
        }
        goto start_compile_server;
    } else if (ei_decode_tuple_header(reply.buff, &dec_index, &dec_size) == 0) {
        atom[0] = '\0';
        if (dec_size >= 2) {
            ei_decode_atom(reply.buff, &dec_index, atom);
        }
        if (dec_size == 2) {
            if (strcmp(atom, "ok") == 0) {
                char* output = decode_binary(reply.buff, &dec_index, &dec_size);
                if (debug) {
                    fprintf(stderr, "called server for %s => ok\n", source_file);
                }
                if (output) {
                    fwrite(output, dec_size, 1, stdout);
                    exit(0);
                }
            }
        } else if (dec_size == 3 && strcmp(atom, "error") == 0) {
            int std_size, err_size;
            char* std;
            char* err;

            if (debug) {
                fprintf(stderr, "called server for %s => error\n", source_file);
            }
            std = decode_binary(reply.buff, &dec_index, &std_size);
            err = decode_binary(reply.buff, &dec_index, &err_size);
            if (std && err) {
                fwrite(err, err_size, 1, stderr);
                fwrite(std, std_size, 1, stdout);
                exit(1);
            }
        }
    }

    /*
     * Unrecognized term, probably because the node was shutting down.
     */

    if (debug > 1) {
        fprintf(stderr, "unrecognized term returned by compilation server:\n");
        dec_index = 0;
        ei_print_term(stderr, reply.buff, &dec_index);
        putc('\n', stderr);
    }

 start_compile_server:
    *strchr(remote, '@') = '\0';
    start_compile_server(remote, argv);
}

static void
encode_env(ei_x_buff* buf)
{
    char* env_names[] = {"ERL_AFLAGS",
                         "ERL_FLAGS",
                         "ERL_ZFLAGS",
                         "ERL_COMPILER_OPTIONS",
                         "ERL_LIBS",
                         "ERLC_CONFIGURATION",
                         0};
    char** p = env_names;
    while (p[0]) {
        char* val;

        if ((val = get_env(p[0])) != 0) {
            ei_x_encode_list_header(buf, 1);
            ei_x_encode_tuple_header(buf, 2);
            ei_x_encode_string(buf, p[0]);
            ei_x_encode_string(buf, val);
        }
        p++;
    }
    ei_x_encode_empty_list(buf);
}

#ifndef __WIN32__
static char*
find_executable(char* progname)
{
    char* path;
    char* start_path;
    char* real_name;
    char buf[PATH_MAX];
    size_t len_component;
    size_t len_prog;

    if (strchr(progname, '/')) {
        return progname;
    }

    len_prog = strlen(progname);

    if (!(path = getenv("PATH"))) {
        path = "/bin:/usr/bin";
    }

    do {
        for (start_path = path; *path != '\0' && *path != ':'; path++) {
            ;
        }
        if (start_path == path) {
            start_path = ".";
            len_component = 1;
        } else {
            len_component = path - start_path;
        }
        memcpy(buf, start_path, len_component);
        buf[len_component] = '/';
        memcpy(buf + len_component + 1, progname, len_prog);
        buf[len_component + len_prog + 1] = '\0';
        if ((real_name = safe_realpath(buf)) != 0) {
            struct stat s;
            if (stat(real_name, &s) == 0 && s.st_mode & S_IFREG) {
                return real_name;
            }
        }
    } while (*path++ == ':');
    return progname;
}

static char*
safe_realpath(char* file)
{
    /*
     * Always allocate a buffer for the result of realpath().
     * realpath() on old versions of MacOS X will crash if the buffer
     * argument is NULL, and realpath() will fail on old versions of
     * Solaris.
     */
    char* real_name = emalloc(PATH_MAX + 1);
    return realpath(file, real_name);
}
#endif

static char*
get_encoding(void)
{
#ifdef __WIN32__
    return "latin1";
#else
    char* p;
    p = get_env("LC_ALL");
    if (!p) {
        p = get_env("LC_CTYPE");
    }
    if (!p) {
        p = get_env("LANG");
    }
    if (!p) {
        return "latin1";
    } else {
        return strstr(p, "UTF-8") ? "utf8" : "latin1";
    }
#endif
}

static char*
decode_binary(const char* buf, int* dec_index, int* dec_size)
{
    int dec_type;
    char* bin;

    ei_get_type(buf, dec_index, &dec_type, dec_size);
    bin = emalloc(*dec_size);
    if (ei_decode_binary(buf, dec_index, bin, NULL) < 0) {
        return NULL;
    }
    return bin;
}

static void
start_compile_server(char* node_name, char** argv)
{
    char* eargv[100];
    int eargc = 0;
    char* progname = argv[0];

    while (strcmp(argv[0], "-mode") != 0) {
        eargv[eargc++] = *argv++;
    }
    PUSH2("-boot", "no_dot_erlang");
    PUSH2("-sname", node_name);
    PUSH2("-setcookie", "erlc_compile_server_cookie");
    PUSH("-hidden");
    PUSH("-detached");
    PUSH3("-kernel", "start_compile_server", "true");

    /*
     * If this is an older Erlang system (before 22.1) that does not
     * support the compile server, terminate immediately.
     */
    PUSH2("-eval", "is_pid(whereis(erl_compile_server)) orelse halt(1)");

    PUSH(NULL);

    if (debug == 1) {
        fprintf(stderr, "starting compile server %s\n", node_name);
    } else if (debug > 1) {
	int i = 0;
        fprintf(stderr, "starting compile server %s:\n", node_name);
	while (eargv[i] != NULL) {
	    fprintf(stderr, "%s ", eargv[i++]);
        }
        putc('\n', stderr);
    }

#ifdef __WIN32__
    if (my_spawnvp(0, eargv) == -1) {
	fprintf(stderr, "erlc: Error executing '%s': %d", progname,
		GetLastError());
    }
#else
    if (fork() == 0) {
        execvp(eargv[0], eargv);
        error("Error %d executing \'%s\'.", errno, progname);
    }
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

#ifdef HAVE_COPYING_PUTENV
static void
efree(void *p)
{
    free(p);
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


static char*
possibly_unquote(char* arg)
{
#ifndef __WIN32__
    /* Nothing to do if not Windows. */
    return arg;
#else
    char* unquoted;
    char* dstp;

    if (arg[0] != '"') {
        /* Not quoted. Nothing to do. */
        return arg;
    }

    /*
     * Remove the quotes and remove backslashes before quotes.
     */

    unquoted = emalloc(strlen(arg) + 1);
    arg++;
    dstp = unquoted;
    while (*arg) {
        if (arg[0] == '\\' && arg[1] == '"') {
            *dstp++ = '"';
            arg += 2;
        } else {
            *dstp++ = *arg++;
        }
    }
    *--dstp = 0;
    return unquoted;
#endif
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
