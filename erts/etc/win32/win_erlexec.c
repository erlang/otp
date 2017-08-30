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

/*
 * To enable debugging of argument processing etc
 * #define ARGS_HARDDEBUG 1
 * #define HARDDEBUG 1
 */

static HMODULE load_win_beam_dll(wchar_t *name)
{
    HMODULE beam_module;
    beam_module=LoadLibraryW(name);
    if (beam_module == INVALID_HANDLE_VALUE || beam_module == NULL) {
	error("Unable to load emulator DLL\n(%S)",name);
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
set_env(char *key, char *value) /* Both in UTF-8 encoding */
{
    wchar_t *wkey=NULL;
    wchar_t *wvalue=NULL;
    int keylen;
    int valuelen;


    keylen = MultiByteToWideChar(CP_UTF8, 0, key, -1, NULL, 0);
    valuelen = MultiByteToWideChar(CP_UTF8, 0, value, -1, NULL, 0);
    wkey = malloc(keylen*sizeof(wchar_t));
    wvalue = malloc(valuelen*sizeof(wchar_t));
    MultiByteToWideChar(CP_UTF8, 0, key, -1, wkey, keylen);
    MultiByteToWideChar(CP_UTF8, 0, value, -1, wvalue, valuelen);
    if (!SetEnvironmentVariableW( wkey, wvalue))
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
start_win_emulator(char* utf8emu, char *utf8start_prog, char** utf8argv, int start_detached)
{
    int len;
    int argc = 0;

    windowed = 1;
    while (utf8argv[argc] != NULL) {
	++argc;
    }

    if (start_detached) {
	wchar_t *start_prog=NULL;
	int result;
	int i;
	wchar_t **argv;
	close(0);
	close(1);
	close(2);
	
	set_env("ERL_CONSOLE_MODE", "detached");
	set_env(DLL_ENV, utf8emu);

	utf8argv[0] = utf8start_prog;
	utf8argv = fnuttify_argv(utf8argv);

	len = MultiByteToWideChar(CP_UTF8, 0, utf8start_prog, -1, NULL, 0);
	start_prog = malloc(len*sizeof(wchar_t));
	MultiByteToWideChar(CP_UTF8, 0, utf8start_prog, -1, start_prog, len);

	/* Convert utf8argv to multibyte argv */
	argv = malloc((argc+1) * sizeof(wchar_t*));
	for (i=0; i<argc; i++) {
	    len = MultiByteToWideChar(CP_UTF8, 0, utf8argv[i], -1, NULL, 0);
	    argv[i] = malloc(len*sizeof(wchar_t));
	    MultiByteToWideChar(CP_UTF8, 0, utf8argv[i], -1, argv[i], len);
	}
	argv[argc] = NULL;

#ifdef ARGS_HARDDEBUG
	{
	    wchar_t tempbuf[2048] = L"";
	    wchar_t *sbuf;
	    int i;
	    sbuf=tempbuf;
	    sbuf += swprintf(sbuf, 2048, L"utf16: %s\n", start_prog);
	    for (i = 0; i < argc; ++i) {
		sbuf += swprintf(sbuf, 2048, L"|%s|", argv[i]);
	    };
	    sbuf += swprintf(sbuf, 2048, L"\nutf8: \n");
	    for (i = 0; i < argc; ++i) {
		sbuf += swprintf(sbuf, 2048, L"|%S|", utf8argv[i]);
	    };
	    MessageBoxW(NULL, tempbuf, L"respawn args", MB_OK|MB_ICONERROR);
	}
#endif

	result = _wspawnv(_P_DETACH, start_prog, argv);
	free_fnuttified(utf8argv);
	if (result == -1) {
	    error("Failed to execute %S: %s", start_prog, win32_errorstr(_doserrno));
	}
    } else {
	wchar_t *emu=NULL;
#ifdef LOAD_BEAM_DYNAMICALLY
	HMODULE beam_module = NULL;
	len = MultiByteToWideChar(CP_UTF8, 0, utf8emu, -1, NULL, 0);
	emu = malloc(len*sizeof(wchar_t));
	MultiByteToWideChar(CP_UTF8, 0, utf8emu, -1, emu, len);
#ifdef ARGS_HARDDEBUG
	{
	    char sbuf[2048] = "";
	    int i;
	    strcat(sbuf,utf8emu);
	    strcat(sbuf,":");
	    for (i = 0; i < argc; ++i) {
		strcat(sbuf,"|");
		strcat(sbuf, utf8argv[i]);
		strcat(sbuf,"| ");
	    }
	    MessageBox(NULL, sbuf, "erl_start args", MB_OK|MB_ICONERROR);
	}
#endif
	beam_module = load_win_beam_dll(emu);
#endif	
	set_env("ERL_CONSOLE_MODE", "window");
#ifdef LOAD_BEAM_DYNAMICALLY
	(*sys_primitive_init_p)(beam_module);
	(*erl_start_p)(argc,utf8argv);
#else
	erl_start(argc,utf8argv);
#endif
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
start_emulator(char* utf8emu, char *utf8start_prog, char** utf8argv, int start_detached)
{
    static char console_mode[] = "tty:ccc";
    char* fd_type;
    char* title;
    int len;
    int argc = 0;

#ifdef HARDDEBUG
    fprintf(stderr,"utf8emu = %s, start_prog = %s\n", utf8emu, utf8start_prog);
#endif

    fd_type = strchr(console_mode, ':');
    fd_type++;
    _flushall();

    while (utf8argv[argc] != NULL) {
	++argc;
    }

    /*
     * If no console, we will spawn the emulator detached.
     */

    if (start_detached) {
	int result;
	int i;
	wchar_t *start_prog=NULL;
	wchar_t **argv;
	close(0);
	close(1);
	close(2);
	set_env("ERL_CONSOLE_MODE", "detached");
	set_env(DLL_ENV, utf8emu);

	utf8argv[0] = utf8start_prog;
	utf8argv = fnuttify_argv(utf8argv);

	len = MultiByteToWideChar(CP_UTF8, 0, utf8start_prog, -1, NULL, 0);
	start_prog = malloc(len*sizeof(wchar_t));
	MultiByteToWideChar(CP_UTF8, 0, utf8start_prog, -1, start_prog, len);

	/* Convert utf8argv to multibyte argv */
	argv = malloc((argc+1) * sizeof(wchar_t*));
	for (i=0; i<argc; i++) {
	    len = MultiByteToWideChar(CP_UTF8, 0,utf8argv[i], -1, NULL, 0);
	    argv[i] = malloc(len*sizeof(wchar_t));
	    MultiByteToWideChar(CP_UTF8, 0, utf8argv[i], -1, argv[i], len);
	}
	argv[argc] = NULL;

#ifdef ARGS_HARDDEBUG
	{
	    wchar_t buffer[2048];
	    int i;
	    wsprintfW(buffer,L"Start detached [%s]\n",start_prog);
	    for(i=0;argv[i] != NULL;++i) {
		wcscat(buffer,L"|");
		wcscat(buffer,argv[i]);
		wcscat(buffer,L"|\n");
	    }
	    MessageBoxW(NULL, buffer, L"Start detached",MB_OK);
	}
#endif
	result = _wspawnv(_P_DETACH, start_prog, argv);
	free_fnuttified(utf8argv);
	free(start_prog);

	if (result == -1) {
#ifdef ARGS_HARDDEBUG
	    MessageBox(NULL, "_wspawnv failed","Start detached",MB_OK);
#endif
	    return 1;
	}
	SetPriorityClass((HANDLE) result, GetPriorityClass(GetCurrentProcess()));
    } else {
	wchar_t *emu=NULL;
#ifdef LOAD_BEAM_DYNAMICALLY
	HMODULE beam_module;
	len = MultiByteToWideChar(CP_UTF8, 0, utf8emu, -1, NULL, 0);
	emu = malloc(len*sizeof(wchar_t));
	MultiByteToWideChar(CP_UTF8, 0, utf8emu, -1, emu, len);
#ifdef ARGS_HARDDEBUG
	{
	    char sbuf[2048] = "";
	    int i;
	    strcat(sbuf,utf8emu);
	    strcat(sbuf,":");
	    for (i = 0; i < argc; ++i) {
		strcat(sbuf,"|");
		strcat(sbuf, utf8argv[i]);
		strcat(sbuf,"| ");
	    }
	    MessageBox(NULL, sbuf, "erl_start args", MB_OK|MB_ICONERROR);
	}
#endif
	beam_module = load_win_beam_dll(emu);
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
	if (keep_window) {
	    atexit(do_keep_window);
	}
#ifdef ARGS_HARDDEBUG
	{
	    char sbuf[2048] = "";
	    int i;
	    for (i = 0; i < argc; ++i) {
		strcat(sbuf,"|");
		strcat(sbuf, utf8argv[i]);
		strcat(sbuf,"|\n");
	    }
	    MessageBox(NULL, sbuf, "erl_start", MB_OK);
	}
#endif
#ifdef LOAD_BEAM_DYNAMICALLY
	(*sys_primitive_init_p)(beam_module);
	(*erl_start_p)(argc,utf8argv);
#else
	erl_start(argc, utf8argv);
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
