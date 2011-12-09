/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2011. All Rights Reserved.
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
 * Extra support for running the emulator on Windows.
 * Most of this only used when beam is run as a separate process.
 */

#pragma comment(linker,"/manifestdependency:\"type='win32' "\
		"name='Microsoft.Windows.Common-Controls' "\
		"version='6.0.0.0' processorArchitecture='*' "\
		"publicKeyToken='6595b64144ccf1df' language='*'\"")

#include <windows.h>
#include <winuser.h>
#include <wincon.h>
#include <process.h>
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "sys.h"
#include "erl_driver.h"

extern int nohup;
extern int keep_window;
void error(char* format, ...);

/*
 * Local functions.
 */
#define  LOAD_BEAM_DYNAMICALLY 1
static int start(char* emu, char** argv);
static void start_winsock(void);
static char* last_error(void);
static char* last_wsa_error(void);
static char* win32_errorstr(int error);
static int has_console(void);
static char** fnuttify_argv(char **argv);
static void free_fnuttified(char **v);
static int windowed = 0;

#ifdef LOAD_BEAM_DYNAMICALLY
typedef int SysGetKeyFunction(int);
typedef void ErlStartFunction(int, char **);
typedef void SysPrimitiveInitFunction(HMODULE);
static SysGetKeyFunction *sys_get_key_p;
static ErlStartFunction *erl_start_p;
static SysPrimitiveInitFunction *sys_primitive_init_p;

static HMODULE load_win_beam_dll(char *name)
{
    HMODULE beam_module;
    beam_module=LoadLibrary(name);
    if (beam_module == INVALID_HANDLE_VALUE || beam_module == NULL) {
	error("Unable to load emulator DLL\n(%s)",name);
	return NULL;
    }
    sys_get_key_p = (SysGetKeyFunction *) 
	GetProcAddress(beam_module, "sys_get_key");
    erl_start_p = (ErlStartFunction *)
	GetProcAddress(beam_module, "erl_start");
    sys_primitive_init_p = (SysPrimitiveInitFunction *)
	GetProcAddress(beam_module, "sys_primitive_init");
    return beam_module;
}
#endif

#define DLL_ENV "ERL_EMULATOR_DLL"

static void
set_env(char *key, char *value)
{
    if (!SetEnvironmentVariable((LPCTSTR) key, (LPCTSTR) value))
	error("SetEnvironmentVariable(\"%s\", \"%s\") failed!", key, value);
}

static char *
get_env(char *key)
{
    DWORD size = 32;
    char *value = NULL;
    while (1) {
	DWORD nsz;
	if (value)
	    free(value);
	value = malloc(size);
	if (!value)
	    error("GetEnvironmentVariable(\"%s\") failed", key);
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
}

free_env_val(char *value)
{
    if (value)
	free(value);
}


int
start_win_emulator(char* emu, char *start_prog, char** argv, int start_detached)
{
    int result;

    windowed = 1;
    if (start_detached) {
	char *buff;
	close(0);
	close(1);
	close(2);
	
	set_env("ERL_CONSOLE_MODE", "detached");
	set_env(DLL_ENV, emu);

	argv[0] = start_prog;
	argv = fnuttify_argv(argv);
	result = spawnv(_P_DETACH, start_prog, argv);
	free_fnuttified(argv);
    } else {
	int argc = 0;
#ifdef LOAD_BEAM_DYNAMICALLY
	HMODULE beam_module = load_win_beam_dll(emu);
#endif	
	set_env("ERL_CONSOLE_MODE", "window");
	while (argv[argc] != NULL) {
	    ++argc;
	}
#ifdef ARGS_HARDDEBUG
	{
	    char sbuf[2048] = "";
	    int i;
	    for (i = 0; i < argc; ++i) {
		strcat(sbuf,"|");
		strcat(sbuf, argv[i]);
		strcat(sbuf,"| ");
	    }
	    MessageBox(NULL, sbuf, "Werl", MB_OK|MB_ICONERROR);
	}
#endif
#ifdef LOAD_BEAM_DYNAMICALLY
	(*sys_primitive_init_p)(beam_module);
	(*erl_start_p)(argc,argv);
#else
	erl_start(argc, argv);
#endif
	result = 0;
    }
    if (result == -1) {
	error("Failed to execute %s: %s", emu, win32_errorstr(_doserrno));
    }
    return 0;
}

void __cdecl 
do_keep_window(void)
{
    printf("\nPress any key to close window.\n");
#ifdef LOAD_BEAM_DYNAMICALLY
    (*sys_get_key_p)(0);
#else 
    sys_get_key(0);
#endif 
}

int
start_emulator(char* emu, char *start_prog, char** argv, int start_detached)
{
    int result;
    static char console_mode[] = "tty:ccc";
    char* fd_type;
    char* title;

#ifdef HARDDEBUG
    fprintf(stderr,"emu = %s, start_prog = %s\n",emu, start_prog);
#endif

    fd_type = strchr(console_mode, ':');
    fd_type++;
    _flushall();
    
    /*
     * If no console, we will spawn the emulator detached.
     */

    if (start_detached) {
	char *buff;
	close(0);
	close(1);
	close(2);
	set_env("ERL_CONSOLE_MODE", "detached");
	set_env(DLL_ENV, emu);

	argv[0] = start_prog;
	argv = fnuttify_argv(argv);
#ifdef ARGS_HARDDEBUG
	{
	    char buffer[2048];
	    int i;
	    sprintf(buffer,"Start detached [%s]\n",start_prog);
	    for(i=0;argv[i] != NULL;++i) {
		strcat(buffer,"|");
		strcat(buffer,argv[i]);
		strcat(buffer,"|\n");
	    }
	    MessageBox(NULL, buffer,"Start detached",MB_OK);
	}
#endif	    
	result = spawnv(_P_DETACH, start_prog, argv);
	free_fnuttified(argv);
	if (result == -1) {
#ifdef ARGS_HARDDEBUG
	    MessageBox(NULL, "_spawnv failed","Start detached",MB_OK);
#endif
	    return 1;
	}
	SetPriorityClass((HANDLE) result, GetPriorityClass(GetCurrentProcess()));
    } else {
	int argc = 0;
#ifdef LOAD_BEAM_DYNAMICALLY
	HMODULE beam_module = load_win_beam_dll(emu);
#endif	

	/*
	 * Start the emulator.
	 */

	title = get_env("ERL_WINDOW_TITLE");
	if (title) {
	    SetConsoleTitle(title);
	}
	free_env_val(title);
	
	set_env("ERL_CONSOLE_MODE", console_mode);
	while (argv[argc] != NULL) {
	    ++argc;
	}
	if (keep_window) {
	    atexit(do_keep_window);
	}
#ifdef ARGS_HARDDEBUG
	{
	    char sbuf[2048] = "";
	    int i;
	    for (i = 0; i < argc; ++i) {
		strcat(sbuf,"|");
		strcat(sbuf, argv[i]);
		strcat(sbuf,"|\n");
	    }
	    MessageBox(NULL, sbuf, "erl", MB_OK);
	}
#endif
#ifdef LOAD_BEAM_DYNAMICALLY
	(*sys_primitive_init_p)(beam_module);
	(*erl_start_p)(argc,argv);
#else
	erl_start(argc, argv);
#endif
    }
    return 0;
}

void
error(char* format, ...)
{
    char sbuf[2048];
    va_list ap;

    va_start(ap, format);
    vsprintf(sbuf, format, ap);
    va_end(ap);

    if (!windowed && has_console()) {
	fprintf(stderr, "%s\n", sbuf);
    } else {
	MessageBox(NULL, sbuf, "Werl", MB_OK|MB_ICONERROR);
    }
    exit(1);
}

static char*
last_error(void)
{
    return win32_errorstr(GetLastError());
}

/*
 * Returns a human-readable description of the last error.
 * The returned pointer will be valid only as long as last-error()
 * isn't called again.
 */

static char*
win32_errorstr(int error)
{
    static LPTSTR lpBufPtr = NULL;

    if (lpBufPtr)
	LocalFree(lpBufPtr);
    FormatMessage(
		  FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
		  NULL,
		  error,
		  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		  (LPTSTR) &lpBufPtr,
		  0,
		  NULL);
    SetLastError(error);
    return lpBufPtr;
}

static int
has_console(void)
{
    HANDLE handle = CreateFile("CONOUT$", GENERIC_WRITE, FILE_SHARE_WRITE,
			       NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

    if (handle != INVALID_HANDLE_VALUE) {
        CloseHandle(handle);
	return 1;
    } else {
        return 0;
    }
}

static char** fnuttify_argv(char **argv)
{
    char **v;
    char *p;
    char *q;
    int c;
    int i;
    int n;
    int m;

    for (c = 0; argv[c]; ++c)
	;

    v = malloc(sizeof(char *) * (c+1));
    v[c] = NULL;
    for (i = 0; i < c; ++i) {
	p = argv[i];
	n = m = 0;
	while (*p) {
	    if (*p == ' ') {
		m = 2;
	    } else if (*p == '"') {
		m = 2;
		++n;
	    }
	    ++p;
	}
	v[i] = malloc((p - argv[i]) + 1 + n + m);
	p = argv[i];
	q = v[i];
	if (n || m) {
	    if (m) {
		*q++ = '"';
	    }
	    while (*p) {
		if (*p == '"') {
		    *q++ = '\\';
		}
		*q++ = *p++;
	    }
	    if (m) {
		*q++ = '"';
	    }
	    *q = '\0';
	} else {
	    strcpy(q,p);
	}
    }
    return v;
}

static void free_fnuttified(char **v)
{
    char **t = v;

    while(*t) {
	free(*t);
	++t;
    }
    free(v);
}
