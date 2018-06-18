/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2007-2018. All Rights Reserved.
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
 * Purpose: escript front-end.
 */

#include "etc_common.h"

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
#  define LDIRSEPSTR L"\\"
#  define LPATHSEPSTR L";"
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
static void* emalloc(size_t size);
static void efree(void *p);
static char* strsave(char* string);
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
	    efree(wcvalue);
	wcvalue = (wchar_t *) emalloc(size*sizeof(wchar_t));
	SetLastError(0);
	nsz = GetEnvironmentVariableW(wckey, wcvalue, size);
	if (nsz == 0 && GetLastError() == ERROR_ENVVAR_NOT_FOUND) {
	    efree(wcvalue);
	    return NULL;
	}
	if (nsz <= size) {
	    len = WideCharToMultiByte(CP_UTF8, 0, wcvalue, -1, NULL, 0, NULL, NULL);
	    value = emalloc(len*sizeof(char));
	    WideCharToMultiByte(CP_UTF8, 0, wcvalue, -1, value, len, NULL, NULL);
	    efree(wcvalue);
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

/*
 * Find absolute path to this program
 */

#ifdef __WIN32__
static char *
find_prog(char *origpath)
{
    wchar_t relpath[PMAX];
    wchar_t abspath[PMAX];

    if (strlen(origpath) >= PMAX)
        error("Path too long");

    MultiByteToWideChar(CP_UTF8, 0, origpath, -1, relpath, PMAX);

    if (wcsstr(relpath, LDIRSEPSTR) == NULL) {
        /* Just a base name */
	int sz;
        wchar_t *envpath;
	sz = GetEnvironmentVariableW(L"PATH", NULL, 0);
        if (sz) {	    
            /* Try to find the executable in the path */
            wchar_t dir[PMAX];
            wchar_t *beg;
            wchar_t *end;

	    HANDLE dir_handle;	        /* Handle to directory. */
	    wchar_t wildcard[PMAX];	/* Wildcard to search for. */
	    WIN32_FIND_DATAW find_data;	/* Data found by FindFirstFile() or FindNext(). */

            BOOL look_for_sep = TRUE;

	    envpath = (wchar_t *) emalloc(sz * sizeof(wchar_t*));
	    GetEnvironmentVariableW(L"PATH", envpath, sz);
	    beg = envpath;

            while (look_for_sep) {
                end = wcsstr(beg, LPATHSEPSTR);
                if (end != NULL) {
                    sz = end - beg;
                } else {
                    sz = wcslen(beg);
                    look_for_sep = FALSE;
                }
                if (sz >= PMAX) {
                    beg = end + 1;
                    continue;
                }
                wcsncpy(dir, beg, sz);
                dir[sz] = L'\0';
                beg = end + 1;

		swprintf(wildcard, PMAX, L"%s" LDIRSEPSTR L"%s",
			      dir, relpath /* basename */);
		dir_handle = FindFirstFileW(wildcard, &find_data);
		if (dir_handle == INVALID_HANDLE_VALUE) {
		    /* Try next directory in path */
		    continue;
		} else {
		    /* Wow we found the executable. */
		    wcscpy(relpath, wildcard);
		    FindClose(dir_handle);
		    look_for_sep = FALSE;
		    break;
		}
            }
	    efree(envpath);
        }
    }
    
    {
	DWORD size;
	wchar_t *absrest;
	size = GetFullPathNameW(relpath, PMAX, abspath, &absrest);
	if ((size == 0) || (size > PMAX)) {
	    /* Cannot determine absolute path to escript. Try the origin.  */
	    return strsave(origpath);
	} else {
	    char utf8abs[PMAX];
	    WideCharToMultiByte(CP_UTF8, 0, abspath, -1, utf8abs, PMAX, NULL, NULL);
	    return strsave(utf8abs);
	}
    }
}
#else
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

            DIR *dp;             /* Pointer to directory structure. */
            struct dirent* dirp; /* Pointer to directory entry.     */

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
            }
        }
    }
    
    {
        if (!realpath(relpath, abspath)) {
	    /* Cannot determine absolute path to escript. Try the origin.  */
	    return strsave(origpath);
	} else {
	    return strsave(abspath);
	}
    }
}
#endif

static void
append_shebang_args(char* scriptname)
{
    /* Open script file */
    FILE* fd; 
#ifdef __WIN32__
    wchar_t wcscriptname[PMAX];

    MultiByteToWideChar(CP_UTF8, 0, scriptname, -1, wcscriptname, PMAX);
    fd = _wfopen(wcscriptname, L"r");
#else
    fd = fopen (scriptname,"r");
#endif

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

#ifdef __WIN32__
int wmain(int argc, wchar_t **wcargv)
{
    char** argv;
#else
int
main(int argc, char** argv)
{
#endif
    int eargv_size;
    int eargc_base;		/* How many arguments in the base of eargv. */
    char* emulator;
    char* basename;
    char* def_emu_lookup_path;
    char scriptname[PMAX];
    char** last_opt;
    char** first_opt;
    
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

    /*
     * Allocate the argv vector to be used for arguments to Erlang.
     * Arrange for starting to pushing information in the middle of
     * the array, to allow easy addition of commands in the beginning.
     */

    eargv_size = argc*4+1000+LINEBUFSZ/2;
    eargv_base = (char **) emalloc(eargv_size*sizeof(char*));
    eargv = eargv_base;
    eargc = 0;
    eargc_base = eargc;
    eargv = eargv + eargv_size/2;
    eargc = 0;

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
        def_emu_lookup_path = argv[0];
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
        char *absname = find_prog(argv[0]);
#ifdef __WIN32__
	int len = strlen(absname);
	if (len >= 4 && _stricmp(absname+len-4, ".exe") == 0) {
	    absname[len-4] = '\0';
	}
#endif
	erts_snprintf(scriptname, sizeof(scriptname), "%s.escript",
		      absname);
        efree(absname);
        def_emu_lookup_path = scriptname;
    }

    /* Determine path to emulator */
    emulator = get_env("ESCRIPT_EMULATOR");

    if (emulator == NULL) {
	emulator = get_default_emulator(def_emu_lookup_path);
    }

    if (strlen(emulator) >= PMAX)
        error("Value of environment variable ESCRIPT_EMULATOR is too large");

    /*
     * Push initial arguments.
     */

    PUSH(emulator);

    PUSH("+B");
    PUSH2("-boot", "no_dot_erlang");
    PUSH("-noshell");

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
     * Add scriptname to env
     */
    set_env("ESCRIPT_NAME", scriptname);

    /*
     * Invoke Erlang with the collected options.
     */

    PUSH(NULL);
    return run_erlang(eargv[0], eargv);
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
	    strcpy(s+1, "bin" DIRSEPSTR ERL_NAME);
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
