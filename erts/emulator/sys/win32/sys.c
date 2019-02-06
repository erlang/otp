/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
 * system-dependent functions
 *
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_alloc.h"
#include "erl_sys_driver.h"
#include "global.h"
#include "erl_threads.h"
#include "../../drivers/win32/win_con.h"
#include "erl_cpu_topology.h"
#include <malloc.h>

void erts_sys_init_float(void);

void erl_start(int, char**);
void erts_exit(int n, char*, ...);
void erl_error(char*, va_list);

/*
 * Microsoft-specific function to map a WIN32 error code to a Posix errno.
 */
extern void _dosmaperr(DWORD);

#ifdef ERL_RUN_SHARED_LIB
#ifdef __argc
#undef __argc
#endif
#define __argc e_argc
#ifdef __argv
#undef __argv
#endif
#define __argv e_argv
#endif

typedef struct driver_data DriverData;

static void init_console();
static int get_and_remove_option(int* argc, char** argv, const char* option);
static char *get_and_remove_option2(int *argc, char **argv, 
				    const char *option);
static int init_async_io(DriverData *dp, struct async_io* aio, int use_threads);
static void release_async_io(struct async_io* aio, ErlDrvPort);
static void async_read_file(struct async_io* aio, LPVOID buf, DWORD numToRead);
static int async_write_file(struct async_io* aio, LPVOID buf, DWORD numToWrite);
static int get_overlapped_result(struct async_io* aio,
				 LPDWORD pBytesRead, BOOL wait);
static BOOL create_child_process(wchar_t *, HANDLE, HANDLE,
				 HANDLE, LPHANDLE, LPDWORD, BOOL,
				 LPVOID, wchar_t*, unsigned,
				 wchar_t **, int *);
static int create_pipe(LPHANDLE, LPHANDLE, BOOL, BOOL);
static int application_type(const wchar_t* originalName, wchar_t fullPath[MAX_PATH],
			   BOOL search_in_path, BOOL handle_quotes,
			   int *error_return);
static void *build_env_block(const erts_osenv_t *env);

HANDLE erts_service_event;

static erts_tsd_key_t win32_errstr_key;

static erts_atomic_t pipe_creation_counter;

/* Results from application_type(_w) is one of */
#define APPL_NONE 0
#define APPL_DOS  1
#define APPL_WIN3X 2
#define APPL_WIN32 3

static int driver_write(long, HANDLE, byte*, int);
static int create_file_thread(struct async_io* aio, int mode);
static void close_active_handle(DriverData *, HANDLE handle);
static DWORD WINAPI threaded_handle_closer(LPVOID param);
static DWORD WINAPI threaded_reader(LPVOID param);
static DWORD WINAPI threaded_writer(LPVOID param);
static DWORD WINAPI threaded_exiter(LPVOID param);

#ifdef DEBUG
static void debug_console(void);
#endif

BOOL WINAPI ctrl_handler(DWORD dwCtrlType);

#define PORT_BUFSIZ 4096

#define DRV_BUF_ALLOC(SZ) \
  erts_alloc_fnf(ERTS_ALC_T_DRV_DATA_BUF, (SZ))
#define DRV_BUF_REALLOC(P, SZ) \
  erts_realloc_fnf(ERTS_ALC_T_DRV_DATA_BUF, (P), (SZ))
#define DRV_BUF_FREE(P) \
  erts_free(ERTS_ALC_T_DRV_DATA_BUF, (P))

/********************* General functions ****************************/

/*
 * Whether create_pipe() should use a named pipe or an anonymous.
 * (Named pipes are not supported on Windows 95.)
 */

static int max_files = 1024;

static BOOL use_named_pipes;
static BOOL win_console = FALSE;


static OSVERSIONINFO int_os_version;	/* Version information for Win32. */

/*#define USE_CANCELIOEX
    Disabled the use of CancelIoEx as its been seen to cause problem with some
    drivers. Not sure what to blame; faulty drivers or some form of invalid use.
*/
#if defined(USE_CANCELIOEX)
static BOOL (WINAPI *fpCancelIoEx)(HANDLE,LPOVERLAPPED);
#endif

/* This is the system's main function (which may or may not be called "main")
   - do general system-dependent initialization
   - call erl_start() to parse arguments and do other init
*/

static erts_atomic_t sys_misc_mem_sz;

HMODULE beam_module = NULL;

void erl_sys_init();

void erl_sys_args(int* argc, char** argv);

int nohup;
#ifndef __GNUC__
void erts_sys_invalid_parameter_handler(const wchar_t * expression,
                                        const wchar_t * function, 
                                        const wchar_t * file, 
                                        unsigned int line,
                                        uintptr_t pReserved
                                        )
{
#ifdef DEBUG
    fprintf(stderr,
	    "Debug: Invalid parameter\"%ls\" "
	    "(detected in \"%ls\" [%ls:%d]) \n", 
	    (expression) ? expression : L"(unknown)",
	    (function) ? function : L"(unknown)",
	    (file) ? file : L"(unknown)",
	    line);
#endif
    return;
}
#endif

void sys_primitive_init(HMODULE beam)
{
#ifndef __GNUC__
    /* Initialize this module handle (the beam.dll module handle) and 
       take care of the standard library's aggressive invalid parameter 
       handling... */
    _set_invalid_parameter_handler(&erts_sys_invalid_parameter_handler);
#endif
    beam_module = (HMODULE) beam;
}

UWord
erts_sys_get_page_size(void)
{
    return (UWord) 4*1024; /* Guess 4 KB */
}

Uint
erts_sys_misc_mem_sz(void)
{
    Uint res = (Uint) erts_check_io_size();
    res += (Uint) erts_atomic_read_mb(&sys_misc_mem_sz);
    return res;
}

/*
 * Reset the terminal to the original settings on exit
 */
void sys_tty_reset(int exit_code)
{
    if (exit_code == ERTS_ERROR_EXIT)
	ConWaitForExit();
    else
	ConNormalExit();
}

void erl_sys_args(int* argc, char** argv)
{
    char *event_name;

    erts_sys_env_init();

    nohup = get_and_remove_option(argc, argv, "-nohup");

#ifdef DEBUG
    /*
     * Start a debug console if -console option given.
     */

    if (get_and_remove_option(argc, argv, "-console")) {
	debug_console();
    }
#endif

    if (nohup && (event_name = get_and_remove_option2(argc, argv, 
						      "-service_event"))) {
	if ((erts_service_event = 
	     OpenEvent(EVENT_ALL_ACCESS,FALSE,event_name)) == NULL) {
	    erts_fprintf(stderr,
			 "Warning: could not open service event: %s\r\n", 
			 event_name);
	}	    
    } else {
	erts_service_event = NULL;
    }

#ifdef DEBUG
    /*
     * Given the "-threads" option, always use threads instead of
     * named pipes.
     */

    if (get_and_remove_option(argc, argv, "-threads")) {
	use_named_pipes = FALSE;
    }
#endif
}

/*
 * Function returns 1 if we can read from all values in between
 * start and stop.
 */
int
erts_sys_is_area_readable(char *start, char *stop) {
    volatile char tmp;
    __try
    {
        while(start < stop) {
            tmp = *start;
            start++;
        }
    }
    __except(EXCEPTION_EXECUTE_HANDLER)
    {
        return 0;
    }
    return 1;
}

int erts_sys_prepare_crash_dump(int secs)
{
    Port *heart_port;
    Eterm heap[3];
    Eterm *hp = heap;
    Eterm list = NIL;

    heart_port = erts_get_heart_port();

    if (heart_port) {

	list = CONS(hp, make_small(8), list); hp += 2;

	/* send to heart port, CMD = 8, i.e. prepare crash dump =o */
	erts_port_output(NULL, ERTS_PORT_SIG_FLG_FORCE_IMM_CALL, heart_port,
			 heart_port->common.id, list, NULL);

	return 1;
    }

    /* Windows - free file descriptors are hopefully available */
    /* Alarm not used on windows */

    return 0;
}

int erts_set_signal(Eterm signal, Eterm type) {
    return 0;
}

static void
init_console(void)
{
    char* mode = erts_read_env("ERL_CONSOLE_MODE");

    if (!mode || strcmp(mode, "window") == 0) {
	win_console = TRUE;
	ConInit();
	/*nohup = 0;*/
    } else if (strncmp(mode, "tty:", 4) == 0) {
	if (mode[5] == 'c') {
	    setvbuf(stdout, NULL, _IONBF, 0);
	}
	if (mode[6] == 'c') {
	    setvbuf(stderr, NULL, _IONBF, 0);
	}
    }

    erts_free_read_env(mode);
}

int sys_max_files(void) 
{
    return max_files;
}

/*
 * Looks for the given option in the argv vector.  If it is found,
 * it will be removed from the argv vector.
 *
 * If the return value indicates that the option was found and removed,
 * it is the responsibility of the caller to decrement the value of argc.
 *
 * Returns: 0 if the option wasn't found, 1 if it was found
 */

static int
get_and_remove_option(int* argc, char* argv[], const char *option)
{
    int i;

    for (i = 1; i < *argc; i++) {
	if (strcmp(argv[i], option) == 0) {
	    (*argc)--;
	    while (i < *argc) {
		argv[i] = argv[i+1];
		i++;
	    }
	    argv[i] = NULL;
	    return 1;
	}
    }
    return 0;
}

static char *get_and_remove_option2(int *argc, char **argv, 
				    const char *option)
{
    char *ret;
    int i;

    for (i = 1; i < *argc; i++) {
	if (strcmp(argv[i], option) == 0) {
	    if (i+1 < *argc) {
		ret = argv[i+1];
		(*argc) -= 2;
		while (i < *argc) {
		    argv[i] = argv[i+2];
		    i++;
		}
		argv[i] = NULL;
		return ret;
	    }
	}
    }
    return NULL;
}
    

/************************** OS info *******************************/

/* Used by erlang:info/1. */
/* (This code was formerly in drv.XXX/XXX_os_drv.c) */

char os_type[] = "win32";

void
os_flavor(char *namebuf, unsigned size)
{
    switch (int_os_version.dwPlatformId) {
    case VER_PLATFORM_WIN32_WINDOWS:
	strcpy(namebuf, "windows");
	break;
    case VER_PLATFORM_WIN32_NT:
	strcpy(namebuf, "nt");
	break;
    default:			/* Can't happen. */
	strcpy(namebuf, "unknown");
	break;
    }
}

void
os_version(pMajor, pMinor, pBuild)
int* pMajor;			/* Pointer to major version. */
int* pMinor;			/* Pointer to minor version. */
int* pBuild;			/* Pointer to build number. */
{
  *pMajor = int_os_version.dwMajorVersion;
  *pMinor = int_os_version.dwMinorVersion;
  *pBuild = int_os_version.dwBuildNumber;
}

/************************** Port I/O *******************************/

/* I. Common stuff */

/* II. The spawn/fd/vanilla drivers */

/*
 * Definitions for driver flags.
 */

#define DF_OVR_READY	  1	/* Overlapped result is ready. */
#define DF_EXIT_THREAD	  2	/* The thread should exit. */
#define DF_XLAT_CR	  4	/* The thread should translate CRs. */
#define DF_DROP_IF_INVH   8     /* Drop packages instead of crash if
				   invalid handle (stderr) */
#define DF_THREAD_FLUSHED 16	/* The thread should exit. */

#define OV_BUFFER_PTR(dp) ((LPVOID) ((dp)->ov.Internal))
#define OV_NUM_TO_READ(dp) ((dp)->ov.InternalHigh)

/*
 * This data is used to make overlapped I/O operations work on both
 * Windows NT (using true overlapped I/O) and Windows 95 (using threads).
 */

typedef struct async_io {
  unsigned flags;		/* Driver flags, definitions found above. */
  HANDLE thread;		/* If -1, overlapped I/O is used (Windows NT).
				 * Otherwise, it is the handle of the thread used
				 * for simulating overlapped I/O (Windows 95 and
				 * the console for Windows NT).
				 */
  HANDLE fd;			/* Handle for file or pipe. */
  int async_io_active;          /* if true, a close of the file will signal the event in ov */
  OVERLAPPED ov;		/* Control structure for overlapped reading.
				 * When overlapped reading is simulated with
				 * a thread, the fields are used as follows:
				 *   ov.Internal - Read buffer.
				 *   ov.InternalHigh - Number of bytes to read.
				 * See macros above.
				 */
  HANDLE ioAllowed;		/* The thread will wait for this event
				 * before starting a new read or write.
				 */
  HANDLE flushEvent;		/* Used to signal that a flush should be done. */
  HANDLE flushReplyEvent;	/* Used to signal that a flush has been done. */
  DWORD pendingError;		/* Used to delay presentating an error to Erlang
				 * until the check_io function is entered.
				 */
  DWORD bytesTransferred;	/* Bytes read or write in the last operation.
				 * Valid only when DF_OVR_READY is set.
				 */
  DriverData *dp;               /* Pointer to driver data struct which
				   this struct is part of */
} AsyncIo;


/*
 * Input thread for fd_driver (if fd_driver is running).
 */
static AsyncIo* fd_driver_input = NULL;
static BOOL (WINAPI *fpSetHandleInformation)(HANDLE,DWORD,DWORD);

/*
 * This data is used by the spawn and vanilla drivers.
 * There will be one entry for each port, even if the input
 * and output HANDLES are different.  Since handles are not
 * guaranteed to be small numbers in Win32, we cannot index
 * with them.  I.e. the index for each entry is not equal to
 * none of the file handles.
 */

struct driver_data {
    int totalNeeded;		/* Total number of bytes needed to fill
				 * up the packet header or packet. */
    int bytesInBuffer;		/* Number of bytes read so far in
				 * the input buffer.
				 */
    int inBufSize;		/* Size of input buffer. */
    byte *inbuf;		/* Buffer to use for overlapped read. */
    int outBufSize;		/* Size of output buffer. */
    byte *outbuf;		/* Buffer to use for overlapped write. */
    ErlDrvPort port_num;	/* The port handle. */
    int packet_bytes;		/* 0: continuous stream, 1, 2, or 4: the number
				 * of bytes in the packet header.
				 */
    HANDLE port_pid;		/* PID of the port process. */
    AsyncIo in;			/* Control block for overlapped reading. */
    AsyncIo out;		/* Control block for overlapped writing. */
    int report_exit;            /* Do report exit status for the port */
    erts_atomic32_t refc;       /* References to this struct */
};

/* Driver interfaces */
static ErlDrvData spawn_start(ErlDrvPort, char*, SysDriverOpts*);
static ErlDrvData fd_start(ErlDrvPort, char*, SysDriverOpts*);
static ErlDrvData vanilla_start(ErlDrvPort, char*, SysDriverOpts*);
static int spawn_init(void);
static int fd_init(void);
static void fd_stop(ErlDrvData);
static void stop(ErlDrvData);
static void output(ErlDrvData, char*, ErlDrvSizeT);
static void ready_input(ErlDrvData, ErlDrvEvent);
static void ready_output(ErlDrvData, ErlDrvEvent);
static void stop_select(ErlDrvEvent, void*);

struct erl_drv_entry spawn_driver_entry = {
    spawn_init,
    spawn_start,
    stop,
    output,
    ready_input,
    ready_output,
    "spawn",
    NULL, /* finish */
    NULL, /* handle */
    NULL, /* control */
    NULL, /* timeout */
    NULL, /* outputv */
    NULL, /* ready_async */
    NULL, /* flush */
    NULL, /* call */
    NULL, /* event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,	/* ERL_DRV_FLAGs */
    NULL,
    NULL, /* process_exit */
    stop_select
};

#ifdef HARD_POLL_DEBUG
extern void poll_debug_set_active_fd(ErtsSysFdType fd);
extern void poll_debug_read_begin(ErtsSysFdType fd);
extern void poll_debug_read_done(ErtsSysFdType fd, int bytes);
extern void poll_debug_async_initialized(ErtsSysFdType fd);
extern void poll_debug_async_immediate(ErtsSysFdType fd, int bytes);
extern void poll_debug_write_begin(ErtsSysFdType fd);
extern void poll_debug_write_done(ErtsSysFdType fd, int bytes);
#endif

extern int null_func(void);

struct erl_drv_entry fd_driver_entry = {
    fd_init,
    fd_start,
    fd_stop,
    output,
    ready_input,
    ready_output,
    "fd",
    NULL, /* finish */
    NULL, /* handle */
    NULL, /* control */
    NULL, /* timeout */
    NULL, /* outputv */
    NULL, /* ready_async */
    NULL, /* flush */
    NULL, /* call */
    NULL, /* event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,	/* ERL_DRV_FLAGs */
    NULL,
    NULL, /* process_exit */
    stop_select
};

struct erl_drv_entry vanilla_driver_entry = {
    null_func,
    vanilla_start,
    stop,
    output,
    ready_input,
    ready_output,
    "vanilla",
    NULL, /* finish */
    NULL, /* handle */
    NULL, /* control */
    NULL, /* timeout */
    NULL, /* outputv */
    NULL, /* ready_async */
    NULL, /* flush */
    NULL, /* call */
    NULL, /* event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,	/* ERL_DRV_FLAGs */
    NULL,
    NULL, /* process_exit */
    stop_select
};

static ERTS_INLINE void
refer_driver_data(DriverData *dp)
{
#ifdef DEBUG
    erts_aint32_t refc = erts_atomic32_inc_read_nob(&dp->refc);
    ASSERT(refc > 1);
#else
    erts_atomic32_inc_nob(&dp->refc);
#endif
}

static ERTS_INLINE void
unrefer_driver_data(DriverData *dp)
{
    erts_aint32_t refc = erts_atomic32_dec_read_mb(&dp->refc);
    ASSERT(refc >= 0);
    if (refc == 0)
	driver_free(dp);
}

/*
 * Initialises a DriverData structure.
 *
 * Results: Returns a pointer to a DriverData structure, or NULL
 * if the initialsation failed.
 */

static DriverData*
new_driver_data(ErlDrvPort port_num, int packet_bytes, int wait_objs_required, int use_threads)
{
    DriverData* dp;

    DEBUGF(("new_driver_data(%p, pb %d)\n", port_num, packet_bytes));

    dp = driver_alloc(sizeof(DriverData));
    if (!dp)
	return NULL;
    /*
     * We used to test first at all that there is enough room in the
     * array used by WaitForMultipleObjects(), but that is not necessary
     * any more, since driver_select() can't fail.
     */

    erts_atomic32_init_nob(&dp->refc, 1);
    dp->bytesInBuffer = 0;
    dp->totalNeeded = packet_bytes;
    dp->inBufSize = PORT_BUFSIZ;
    dp->inbuf = DRV_BUF_ALLOC(dp->inBufSize);
    if (dp->inbuf == NULL)
	goto buf_alloc_error;
    erts_atomic_add_nob(&sys_misc_mem_sz, dp->inBufSize);
    dp->outBufSize = 0;
    dp->outbuf = NULL;
    dp->port_num = port_num;
    dp->packet_bytes = packet_bytes;
    dp->port_pid = INVALID_HANDLE_VALUE;
    if (init_async_io(dp, &dp->in, use_threads) == -1)
	goto async_io_error1;
    if (init_async_io(dp, &dp->out, use_threads) == -1)
	goto async_io_error2;

    return dp;

async_io_error2:
    release_async_io(&dp->in, dp->port_num);
async_io_error1:
    release_async_io(&dp->out, dp->port_num);

buf_alloc_error:
    driver_free(dp);
    return NULL;
}

static void
release_driver_data(DriverData* dp)
{
#ifdef USE_CANCELIOEX
    if (fpCancelIoEx != NULL) {
	if (dp->in.thread == (HANDLE) -1 && dp->in.fd != INVALID_HANDLE_VALUE) {
	    (*fpCancelIoEx)(dp->in.fd, NULL);
	}
	if (dp->out.thread == (HANDLE) -1 && dp->out.fd != INVALID_HANDLE_VALUE) {
	    (*fpCancelIoEx)(dp->out.fd, NULL);
	}
    }
    else
#endif
    {
	/* This is a workaround for the fact that CancelIo cant cancel
	   requests issued by another thread and that we cant use
	   CancelIoEx as that's only available in Vista etc.
	   R14: Avoid scheduler deadlock by only wait for 10ms, and then spawn
	    a thread that will keep waiting in in order to close handles. */
	HANDLE handles[2];
	int i = 0;
	int timeout = 10;
	if(dp->in.async_io_active && dp->in.fd != INVALID_HANDLE_VALUE) {
	    CloseHandle(dp->in.fd);
	    dp->in.fd = INVALID_HANDLE_VALUE;
	    DEBUGF(("Waiting for the in event thingie"));
	    if (WaitForSingleObject(dp->in.ov.hEvent,timeout) == WAIT_TIMEOUT) {
		close_active_handle(dp, dp->in.ov.hEvent);
		dp->in.ov.hEvent = NULL;
		timeout = 0;
	    }
	    DEBUGF(("...done\n"));
	}
	if(dp->out.async_io_active && dp->out.fd != INVALID_HANDLE_VALUE) {
	    CloseHandle(dp->out.fd);
	    dp->out.fd = INVALID_HANDLE_VALUE;
	    DEBUGF(("Waiting for the out event thingie"));
	    if (WaitForSingleObject(dp->out.ov.hEvent,timeout) == WAIT_TIMEOUT) {
		close_active_handle(dp, dp->out.ov.hEvent);
		dp->out.ov.hEvent = NULL;
	    }
	    DEBUGF(("...done\n"));
	}
    }

    if (dp->inbuf != NULL) {
	ASSERT(erts_atomic_read_nob(&sys_misc_mem_sz) >= dp->inBufSize);
	erts_atomic_add_nob(&sys_misc_mem_sz, -1*dp->inBufSize);
	DRV_BUF_FREE(dp->inbuf);
	dp->inBufSize = 0;
	dp->inbuf = NULL;
    }
    ASSERT(dp->inBufSize == 0);

    if (dp->outbuf != NULL) {
	ASSERT(erts_atomic_read_nob(&sys_misc_mem_sz) >= dp->outBufSize);
	erts_atomic_add_nob(&sys_misc_mem_sz, -1*dp->outBufSize);
	DRV_BUF_FREE(dp->outbuf);
	dp->outBufSize = 0;
	dp->outbuf = NULL;
    }
    ASSERT(dp->outBufSize == 0);

    if (dp->port_pid != INVALID_HANDLE_VALUE) {
	CloseHandle(dp->port_pid);
	dp->port_pid = INVALID_HANDLE_VALUE;
    }

    release_async_io(&dp->in, dp->port_num);
    release_async_io(&dp->out, dp->port_num);

    /*
     * This must be last, because this function might be executed from
     * the exit thread.
     */

    unrefer_driver_data(dp);
}


struct handles_to_be_closed {
    HANDLE handles[MAXIMUM_WAIT_OBJECTS];
    DriverData *drv_data[MAXIMUM_WAIT_OBJECTS];
    unsigned cnt;
};
static struct handles_to_be_closed* htbc_curr = NULL;
CRITICAL_SECTION htbc_lock;

static void close_active_handle(DriverData *dp, HANDLE handle)
{
    struct handles_to_be_closed* htbc;
    int i;
    EnterCriticalSection(&htbc_lock);
    htbc = htbc_curr;
    if (htbc == NULL || htbc->cnt >= MAXIMUM_WAIT_OBJECTS) {
	DWORD tid;
	HANDLE thread;

	htbc = (struct handles_to_be_closed*) erts_alloc(ERTS_ALC_T_DRV_TAB,
							 sizeof(*htbc));
	htbc->handles[0] = CreateAutoEvent(FALSE);
	htbc->drv_data[0] = NULL;
	htbc->cnt = 1;
	thread = (HANDLE *) _beginthreadex(NULL, 0, threaded_handle_closer, htbc, 0, &tid);
	CloseHandle(thread);
    }
    i = htbc->cnt++;
    htbc->handles[i] = handle;
    htbc->drv_data[i] = dp;
    if (dp)
	refer_driver_data(dp); /* Need to keep driver data until we have
				  closed the event; outstanding operation
				  might write into it.. */
    SetEvent(htbc->handles[0]);
    htbc_curr = htbc;
    LeaveCriticalSection(&htbc_lock);
}

static DWORD WINAPI
threaded_handle_closer(LPVOID param)
{
    struct handles_to_be_closed* htbc = (struct handles_to_be_closed*) param;
    unsigned ix;
    DWORD res;
    DEBUGF(("threaded_handle_closer %p started\r\n", htbc));
    EnterCriticalSection(&htbc_lock);
    for (;;) {
	{
	    HANDLE* handles = htbc->handles;
	    unsigned cnt = htbc->cnt;
	    DWORD timeout = (htbc == htbc_curr) ? INFINITE : 10*1000;

	    LeaveCriticalSection(&htbc_lock);
	    DEBUGF(("threaded_handle_closer %p waiting for %d handles\r\n", htbc, cnt));
	    res = WaitForMultipleObjects(cnt, handles, FALSE, timeout);
	}
	EnterCriticalSection(&htbc_lock);
	switch (res) {
	case WAIT_OBJECT_0:
	case WAIT_TIMEOUT:
	    break; /* got some more handles to wait for maybe */
	default:
	    ix = res - WAIT_OBJECT_0;
	    if (ix > 0 && ix < htbc->cnt) {
		int move_ix;
		CloseHandle(htbc->handles[ix]);
		if (htbc->drv_data[ix])
		    unrefer_driver_data(htbc->drv_data[ix]);
		move_ix = --htbc->cnt;
		htbc->handles[ix] = htbc->handles[move_ix];
		htbc->drv_data[ix] = htbc->drv_data[move_ix];
	    }
	}
	if (htbc != htbc_curr) {
	    if (htbc->cnt == 1) { /* no real handles left */
		break;
	    }
	    /* The thread with most free slots will be "current" */
	    if (htbc->cnt < htbc_curr->cnt) {
		htbc_curr = htbc;
		DEBUGF(("threaded_handle_closer %p made current\r\n", htbc));
	    }
	}
    }
    LeaveCriticalSection(&htbc_lock);
    CloseHandle(htbc->handles[0]);
    ASSERT(!htbc->drv_data[0]);
    erts_free(ERTS_ALC_T_DRV_TAB, htbc);
    DEBUGF(("threaded_handle_closer %p terminating\r\n", htbc));
    return 0;
}

/*
 * Stores input and output file descriptors in the DriverData structure,
 * and calls driver_select().
 *
 * This function fortunately can't fail!
 */

static ErlDrvData
set_driver_data(DriverData* dp, HANDLE ifd, HANDLE ofd, int read_write, int report_exit)
{
    int result;

    dp->in.fd = ifd;
    dp->out.fd = ofd;
    dp->report_exit = report_exit;

    if (read_write & DO_READ) {
	result = driver_select(dp->port_num, (ErlDrvEvent)dp->in.ov.hEvent,
			       ERL_DRV_READ|ERL_DRV_USE, 1);
	ASSERT(result != -1);
	async_read_file(&dp->in, dp->inbuf, dp->inBufSize);
    }

    if (read_write & DO_WRITE) {
	result = driver_select(dp->port_num, (ErlDrvEvent)dp->out.ov.hEvent,
			       ERL_DRV_WRITE|ERL_DRV_USE, 1);
	ASSERT(result != -1);
    }
    return (ErlDrvData) dp;
}

static ErlDrvData
reuse_driver_data(DriverData *dp, HANDLE ifd, HANDLE ofd, int read_write, ErlDrvPort port_num)
{
    int result;

    dp->port_num = port_num;
    dp->in.fd = ifd;
    dp->out.fd = ofd;
    dp->report_exit = 0;

    if (read_write & DO_READ) {
	result = driver_select(dp->port_num, (ErlDrvEvent)dp->in.ov.hEvent,
			       ERL_DRV_READ|ERL_DRV_USE, 1);
	ASSERT(result != -1);
    }

    if (read_write & DO_WRITE) {
	result = driver_select(dp->port_num, (ErlDrvEvent)dp->out.ov.hEvent,
			       ERL_DRV_WRITE|ERL_DRV_USE, 1);
	ASSERT(result != -1);
    }
    return (ErlDrvData) dp;
}

/*
 * Initialises an AsyncIo structure.
 */

static int
init_async_io(DriverData *dp, AsyncIo* aio, int use_threads)
{
    aio->dp = dp;
    aio->flags = 0;
    aio->thread = (HANDLE) -1;
    aio->fd = INVALID_HANDLE_VALUE;
    aio->ov.hEvent = NULL;
    aio->ov.Offset = 0L;
    aio->ov.OffsetHigh = 0L;
    aio->ioAllowed = NULL;
    aio->flushEvent = NULL;
    aio->flushReplyEvent = NULL;
    aio->pendingError = 0;
    aio->bytesTransferred = 0;
    aio->async_io_active = 0;
    aio->ov.hEvent = CreateManualEvent(FALSE);
    if (aio->ov.hEvent == NULL)
	return -1;
    if (use_threads) {
	OV_BUFFER_PTR(aio) = NULL;
	OV_NUM_TO_READ(aio) = 0;
	aio->ioAllowed = CreateAutoEvent(FALSE);
	if (aio->ioAllowed == NULL)
	    return -1;
	aio->flushEvent = CreateAutoEvent(FALSE);
	if (aio->flushEvent == NULL)
	  return -1;
	aio->flushReplyEvent = CreateAutoEvent(FALSE);
	if (aio->flushReplyEvent == NULL)
	  return -1;
    }
    return 0;
}

/*
 * Releases everything allocated in an AsyncIo structure.
 */  

static void
release_async_io(AsyncIo* aio, ErlDrvPort port_num)
{
    aio->flags = 0;

    if (aio->thread != (HANDLE) -1)
	CloseHandle(aio->thread);
    aio->thread = (HANDLE) -1;

    if (aio->fd != INVALID_HANDLE_VALUE)
	CloseHandle(aio->fd);
    aio->fd = INVALID_HANDLE_VALUE;

    if (aio->ov.hEvent != NULL)
	CloseHandle(aio->ov.hEvent);

    aio->ov.hEvent = NULL;

    if (aio->ioAllowed != NULL)
	CloseHandle(aio->ioAllowed);
    aio->ioAllowed = NULL;

    if (aio->flushEvent != NULL)
	CloseHandle(aio->flushEvent);
    aio->flushEvent = NULL;

    if (aio->flushReplyEvent != NULL)
	CloseHandle(aio->flushReplyEvent);
    aio->flushReplyEvent = NULL;
}

/* ----------------------------------------------------------------------
 * async_read_file --
 *	Initiaties an asynchronous file read, or simulates that using
 *	the thread associated with this driver data.  To get the results,
 *	call get_overlapped_result().
 *
 * Results:
 *	None.
 * ----------------------------------------------------------------------
 */

static void
async_read_file(AsyncIo* aio, LPVOID buf, DWORD numToRead)
{
    aio->pendingError = NO_ERROR;
#ifdef HARD_POLL_DEBUG
    poll_debug_async_initialized(aio->ov.hEvent);
#endif
    if (aio->thread != (HANDLE) -1) {
	DEBUGF(("async_read_file: signaling thread 0x%x, event 0x%x\n",
		aio->thread, aio->ioAllowed));
	OV_BUFFER_PTR(aio) = buf;
	OV_NUM_TO_READ(aio) = numToRead;
	ResetEvent(aio->ov.hEvent);
	SetEvent(aio->ioAllowed);
    } else {
	aio->async_io_active = 1; /* Will get 0 when the event actually happened */
	if (ReadFile(aio->fd, buf, numToRead,
		     &aio->bytesTransferred, &aio->ov)) {
	    DEBUGF(("async_read_file: ReadFile() suceeded: %d bytes\n",
		    aio->bytesTransferred));
#ifdef HARD_POLL_DEBUG
	    poll_debug_async_immediate(aio->ov.hEvent, aio->bytesTransferred);
#endif
	    aio->flags |= DF_OVR_READY;
	    SetEvent(aio->ov.hEvent);
	} else {
	    DWORD error = GetLastError();
	    if (error != ERROR_IO_PENDING) {
#ifdef HARD_POLL_DEBUG
		poll_debug_async_immediate(aio->ov.hEvent, 0);
#endif
		aio->pendingError = error;
		SetEvent(aio->ov.hEvent);
	    }
	    DEBUGF(("async_read_file: ReadFile() -> %s\n", win32_errorstr(error)));
	}
    }
}

/* ----------------------------------------------------------------------
 * async_write_file --
 *	Initiaties an asynchronous file write, or simulates that using
 *	the output thread associated with this driver data. 
 *	To get the results, call get_overlapped_result().
 *
 * Results:
 *	None.
 * ----------------------------------------------------------------------
 */
static int
async_write_file(AsyncIo* aio,		/* Pointer to async control block. */
		 LPVOID buf,		/* Pointer to buffer with data to write. */
		 DWORD numToWrite)	/* Number of bytes to write. */
{
    aio->pendingError = NO_ERROR;
    if (aio->thread != (HANDLE) -1) {
	DEBUGF(("async_write_file: signaling thread 0x%x, event 0x%x\n",
		aio->thread, aio->ioAllowed));
	OV_BUFFER_PTR(aio) = buf;
	OV_NUM_TO_READ(aio) = numToWrite;
	ResetEvent(aio->ov.hEvent);
	SetEvent(aio->ioAllowed);
    } else {
	aio->async_io_active = 1; /* Will get 0 when the event actually happened */
	if (WriteFile(aio->fd, buf, numToWrite,
		      &aio->bytesTransferred, &aio->ov)) {
	    DEBUGF(("async_write_file: WriteFile() suceeded: %d bytes\n",
		    aio->bytesTransferred));
	    aio->async_io_active = 0; /* The event will not be signalled */
	    ResetEvent(aio->ov.hEvent);
	    return TRUE;
	} else {
	    DWORD error = GetLastError();
	    if (error != ERROR_IO_PENDING) {
		aio->pendingError = error;
		SetEvent(aio->ov.hEvent);
	    }
	    DEBUGF(("async_write_file: WriteFile() -> %s\n", win32_errorstr(error)));
	}
    }
    return FALSE;
}

/* ----------------------------------------------------------------------
 * get_overlapped_result --
 *
 * Results:
 *	Returns the error code for the overlapped result, or NO_ERROR
 *	if no error.
 * ----------------------------------------------------------------------
 */
static int
get_overlapped_result(AsyncIo* aio,		/* Pointer to async control block. */
		      LPDWORD pBytesRead,	/* Where to place the number of bytes
						 * transferred.
						 */
		      BOOL wait			/* If true, wait until result is ready. */
		      )
{
    DWORD error = NO_ERROR;	/* Error status from last function. */

    if (aio->thread != (HANDLE) -1) {

	/*
	 * Simulate overlapped io with a thread.
	 */
	DEBUGF(("get_overlapped_result: about to wait for event 0x%x\n",
		aio->ov.hEvent));
	error = WaitForSingleObject(aio->ov.hEvent, wait ? INFINITE : 0);
	switch (error) {
	case WAIT_OBJECT_0:
	    error = aio->pendingError;
	    aio->pendingError = NO_ERROR;
	    *pBytesRead = aio->bytesTransferred;
	    ResetEvent(aio->ov.hEvent);
	    DEBUGF(("get_overlapped_result -> %s\n",
		    win32_errorstr(error)));
	    return error;
	case WAIT_TIMEOUT:
	    DEBUGF(("get_overlapped_result -> %s\n",
		    ERROR_IO_INCOMPLETE));
	    return ERROR_IO_INCOMPLETE;
	case WAIT_FAILED:		/* XXX: Shouldn't happen? */
	    error = GetLastError();
	    DEBUGF(("get_overlapped_result (WAIT_FAILED) -> %s\n",
		    win32_errorstr(error)));
	    return error;
	}
    } else if (aio->pendingError != NO_ERROR) { /* Pending error. */
	error = aio->pendingError;
	aio->pendingError = NO_ERROR;
	ResetEvent(aio->ov.hEvent);
	DEBUGF(("get_overlapped_result: pending error: %s\n",
		win32_errorstr(error)));
	return error;
    } else if (aio->flags & DF_OVR_READY) { /* Operation succeded. */
	aio->flags &= ~DF_OVR_READY;
	*pBytesRead = aio->bytesTransferred;
	ResetEvent(aio->ov.hEvent);
	DEBUGF(("get_overlapped_result: delayed success: %d bytes\n",
		aio->bytesTransferred));
    } else if (!GetOverlappedResult(aio->fd, &aio->ov, pBytesRead, wait)) {
	error = GetLastError();
	ResetEvent(aio->ov.hEvent);
	DEBUGF(("get_overlapped_result: error: %s\n", win32_errorstr(error)));
	return error;
    } else {			/* Success. */
	DEBUGF(("get_overlapped_result: success\n"));
	ResetEvent(aio->ov.hEvent);
    }
    return NO_ERROR;
}
  
static int
fd_init(void)
{
    char kernel_dll_name[] = "kernel32";
    HMODULE module;
    module = GetModuleHandle(kernel_dll_name);
    fpSetHandleInformation = (module != NULL) ? 
	(BOOL (WINAPI *)(HANDLE,DWORD,DWORD)) 
	GetProcAddress(module,"SetHandleInformation") : 
	NULL;

    return 0;
}
static int
spawn_init(void)
{
    int i;
#if defined(USE_CANCELIOEX)
    HMODULE module = GetModuleHandle("kernel32");
    fpCancelIoEx = (BOOL (WINAPI *)(HANDLE,LPOVERLAPPED))
	((module != NULL) ? GetProcAddress(module,"CancelIoEx") : NULL);
    DEBUGF(("fpCancelIoEx = %p\r\n", fpCancelIoEx));
#endif

    return 0;
}

static ErlDrvData
spawn_start(ErlDrvPort port_num, char* utf8_name, SysDriverOpts* opts)
{
    HANDLE hToChild = INVALID_HANDLE_VALUE; /* Write handle to child. */
    HANDLE hFromChild = INVALID_HANDLE_VALUE; /* Read handle from child. */
    HANDLE hChildStdin = INVALID_HANDLE_VALUE;		/* Child's stdin. */
    HANDLE hChildStdout = INVALID_HANDLE_VALUE;	/* Child's stout. */
    HANDLE hChildStderr = INVALID_HANDLE_VALUE;	/* Child's sterr. */
    DWORD pid;
    int close_child_stderr = 0;
    DriverData* dp;		/* Pointer to driver data. */
    ErlDrvData retval = ERL_DRV_ERROR_GENERAL; /* Return value. */
    int ok;
    int neededSelects = 0;
    SECURITY_ATTRIBUTES sa = {sizeof(SECURITY_ATTRIBUTES), NULL, TRUE};
    int errno_return = -1;
    wchar_t *name;
    int len;

    if (opts->read_write & DO_READ)
	neededSelects++;
    if (opts->read_write & DO_WRITE)
	neededSelects++;

    if ((dp = new_driver_data(port_num, opts->packet_bytes, neededSelects,
			      !use_named_pipes)) == NULL)
	return ERL_DRV_ERROR_GENERAL;

    /*
     * Create two pipes to communicate with the port program.
     */

    if (opts->read_write & DO_READ) {
	if (!create_pipe(&hFromChild, &hChildStdout, FALSE, 
			 opts->overlapped_io))
	    goto error;
    } else {
	hChildStdout = CreateFile("nul", GENERIC_WRITE, 0,
				  &sa, OPEN_EXISTING,
				  FILE_ATTRIBUTE_NORMAL, NULL);
	DEBUGF(("Created nul file for hChildStdout = %d\n",hChildStdout));
    }
    if (opts->read_write & DO_WRITE) {
	if (!create_pipe(&hChildStdin, &hToChild, TRUE, opts->overlapped_io)) {
	    CloseHandle(hFromChild);
	    hFromChild = INVALID_HANDLE_VALUE;
	    CloseHandle(hChildStdout);
	    goto error;
	}
    } else {
	hChildStdin = CreateFile("nul", GENERIC_READ, 0,
				 &sa, OPEN_EXISTING,
				 FILE_ATTRIBUTE_NORMAL, NULL);
	DEBUGF(("Created nul file for hChildStdin = %d\n",hChildStdin));
    }	

    /*
     * Make sure that standard error is valid handle, because a Command Prompt
     * window not work properly otherwise.  We leave standard error alone if
     * it is okay and no redirection was specified.
     */
    hChildStderr = GetStdHandle(STD_ERROR_HANDLE);
    if (opts->redir_stderr) {
	hChildStderr = hChildStdout;
    } else if (hChildStderr == INVALID_HANDLE_VALUE || hChildStderr == 0) {
	hChildStderr = CreateFile("nul", GENERIC_WRITE, 0, &sa, OPEN_EXISTING,
				  FILE_ATTRIBUTE_NORMAL, NULL);
	close_child_stderr = 1;
    }
    if (fpSetHandleInformation != NULL) {
	(*fpSetHandleInformation)(hChildStderr, HANDLE_FLAG_INHERIT, 1);
    }
    /*
     * Spawn the port program.
     */

    if ((len = MultiByteToWideChar(CP_UTF8, 0, utf8_name, -1, NULL, 0)) > 0) {
	name = erts_alloc(ERTS_ALC_T_TMP, len*sizeof(wchar_t));
	MultiByteToWideChar(CP_UTF8, 0, utf8_name, -1, name, len);
    } else { /* Not valid utf-8, just convert byte to wchar */
	int i;
	len  = strlen(utf8_name);
	name = erts_alloc(ERTS_ALC_T_TMP, (1+len)*sizeof(wchar_t));
	for(i=0; i<len; i++) {
	    name[i] = (wchar_t) utf8_name[i];
	}
	name[i] = L'\0';
    }
    DEBUGF(("Spawning \"%S\"\n", name));

    {
        void *environment_block = build_env_block(&opts->envir);

        ok = create_child_process(name,
                                  hChildStdin,
                                  hChildStdout,
                                  hChildStderr,
                                  &dp->port_pid,
                                  &pid,
                                  opts->hide_window,
                                  environment_block,
                                  (wchar_t *) opts->wd,
                                  opts->spawn_type,
                                  (wchar_t **) opts->argv,
                                  &errno_return);

        CloseHandle(hChildStdin);
        CloseHandle(hChildStdout);

        if (close_child_stderr && hChildStderr != INVALID_HANDLE_VALUE &&
            hChildStderr != 0) {
            CloseHandle(hChildStderr);
        }

        erts_free(ERTS_ALC_T_TMP, environment_block);
        erts_free(ERTS_ALC_T_TMP, name);
    }

    if (!ok) {
	dp->port_pid = INVALID_HANDLE_VALUE;
	if (errno_return >= 0) {
	    retval = ERL_DRV_ERROR_ERRNO;
	} 
    } else {
	if (!use_named_pipes) {
	    if ((opts->read_write & DO_READ) &&
		!create_file_thread(&dp->in, DO_READ))
		goto error;
	    if ((opts->read_write & DO_WRITE) &&
		!create_file_thread(&dp->out, DO_WRITE)) {
		dp->in.flags = DF_EXIT_THREAD;
		SetEvent(dp->in.ioAllowed);
		WaitForSingleObject(dp->in.thread, INFINITE);
		dp->in.thread = (HANDLE) -1;
		goto error;
	    }
	}
#ifdef HARD_POLL_DEBUG
	if (strncmp(name,"inet_gethost",12) == 0) {
	    erts_printf("Debugging \"%s\"\n", name);
	    poll_debug_set_active_fd(dp->in.ov.hEvent);
	}
#endif
	retval = set_driver_data(dp, hFromChild, hToChild, opts->read_write,
				 opts->exit_status);
	if (retval != ERL_DRV_ERROR_GENERAL && retval != ERL_DRV_ERROR_ERRNO) {
            /* We assume that this cannot generate a negative number */
            erl_drv_set_os_pid(port_num, pid);
	}
    }
    
    if (retval != ERL_DRV_ERROR_GENERAL && retval != ERL_DRV_ERROR_ERRNO)
	return retval;
    
 error:
    if (hFromChild != INVALID_HANDLE_VALUE)
	CloseHandle(hFromChild);
    if (hToChild != INVALID_HANDLE_VALUE)
	CloseHandle(hToChild);
    release_driver_data(dp);
    if (retval == ERL_DRV_ERROR_ERRNO) {
	errno = errno_return;
    }
    return retval;
}

struct __build_env_state {
    WCHAR *next_variable;
};

static void build_env_foreach(void *_state, const erts_osenv_data_t *key,
                              const erts_osenv_data_t *value)
{
    struct __build_env_state *state = (struct __build_env_state*)(_state);

    sys_memcpy(state->next_variable, key->data, key->length);
    state->next_variable += (int)key->length / sizeof(WCHAR);
    *state->next_variable++ = L'=';

    sys_memcpy(state->next_variable, value->data, value->length);
    state->next_variable += (int)value->length / sizeof(WCHAR);
    *state->next_variable++ = L'\0';
}

/* Builds an environment block suitable for CreateProcessW. */
static void *build_env_block(const erts_osenv_t *env) {
    struct __build_env_state build_state;
    WCHAR *env_block;

    env_block = erts_alloc(ERTS_ALC_T_TMP, env->content_size +
        (env->variable_count * sizeof(L"=\0") + sizeof(L'\0')));

    build_state.next_variable = env_block;

    erts_osenv_foreach_native(env, &build_state, build_env_foreach);

    (*build_state.next_variable) = L'\0';

    return env_block;
}

static int
create_file_thread(AsyncIo* aio, int mode)
{
    DWORD tid;			/* Id for thread. */

    refer_driver_data(aio->dp);
    aio->thread = (HANDLE)
	_beginthreadex(NULL, 0, 
		       (mode & DO_WRITE) ? threaded_writer : threaded_reader,
		       aio, 0, &tid);
    if (aio->thread != (HANDLE) -1)
	return 1;
    unrefer_driver_data(aio->dp);
    return 0;
}

/* 
 *  A helper function used by create_child_process().
 *  Parses a command line with arguments and returns the length of the
 *  first part containing the program name.
 *  Example: input = "\"Program Files\"\\erl arg1 arg2"
 *  gives 19 as result.
 *  The length returned is equivalent with length(argv[0]) if the
 *  command line should have been prepared by _setargv for the main function
*/
int parse_command(wchar_t* cmd){
#define NORMAL 2
#define STRING 1
#define STOP 0
    int i =0;
    int state = NORMAL;
    while (cmd[i]) {
	switch (cmd[i]) {
	case L'"':
	    if (state == NORMAL) 
		state = STRING;
	    else
		state = NORMAL;
	    break;
	case L'\\':
	    if ((state == STRING) && (cmd[i+1]==L'"'))
		i++;
	    break;
	case L' ':
	    if (state == NORMAL)
		state = STOP;
	    break;
	default:
	    break;
	}
	if (state == STOP) {
	    return i;
	}
	i++;
    }
    return i;
}


/*
 * Translating of command line arguments to correct format. In the examples
 * below the '' are not part of the actual string. 
 * 'io:format("hello").' -> 'io:format(\"hello\").'
 * 'io:format("is anybody in there?").' -> '"io:format(\"is anybody in there?\")."'
 * 'Just nod if you can hear me.' -> '"Just nod if you can hear me."'
 * 'Is there ""anyone at home?' -> '"Is there \"\"anyone at home?"'
 * 'Relax."' -> 'Relax.\"'
 *
 * If new == NULL we just calculate the length.
 *
 * The reason for having to quote all of the is because CreateProcessW removes
 * one level of escaping since it takes a single long command line rather
 * than the argument chunks that unix uses.
 */
static int escape_and_quote(wchar_t *str, wchar_t *new, BOOL *quoted) {
    int i, j = 0;
    if (new == NULL)
        *quoted = FALSE;
    else if (*quoted)
        new[j++] = L'"';
    for ( i = 0; str[i] != L'\0'; i++,j++) {
        if (str[i] == L' ' && new == NULL && *quoted == FALSE) {
	    *quoted = TRUE;
	    j++;
	}
	/* check if we have to escape quotes */
	if (str[i] == L'"') {
	    if (new) new[j] = L'\\';
	    j++;
	}
	if (new) new[j] = str[i];
    }
    if (*quoted) {
        if (new) new[j] = L'"';
	j++;
    }
    return j;
}

/*
 *----------------------------------------------------------------------
 *
 * create_child_process --
 *
 *	Create a child process that has pipes as its 
 *	standard input, output, and error.  The child process runs
 *	synchronously under Win32s and asynchronously under Windows NT
 *	and Windows 95, and runs with the same environment variables
 *	as the creating process.
 *
 *	The complete Windows search path is searched to find the specified 
 *	executable.  If an executable by the given name is not found, 
 *	automatically tries appending ".com", ".exe", and ".bat" to the 
 *	executable name.
 *
 * Results:
 *	The return value is FALSE if there was a problem creating the child process.  
 *      Otherwise, the return value is 0 and *phPid is
 *	filled with the process id of the child process.
 * 
 * Side effects:
 *	A process is created.
 *	
 *----------------------------------------------------------------------
 */

static BOOL
create_child_process
(
 wchar_t *origcmd,  /* Command line for child process (including
		  * name of executable). Or whole executable if st is
		  * ERTS_SPAWN_EXECUTABLE
		  */
 HANDLE hStdin,  /* The standard input handle for child. */
 HANDLE hStdout, /* The standard output handle for child. */ 
 HANDLE hStderr, /* The standard error handle for child. */
 LPHANDLE phPid, /* Pointer to variable to received Process handle. */
 LPDWORD pdwID,   /* Pointer to variable to received Process ID */
 BOOL hide,      /* Hide the window unconditionally. */
 LPVOID env,     /* Environment for the child */
 wchar_t *wd,      /* Working dir for the child */
 unsigned st,    /* Flags for spawn, tells us how to interpret origcmd */
 wchar_t **argv,     /* Argument vector if given. */
 int *errno_return /* Place to put an errno in in case of failure */
 )
{
    PROCESS_INFORMATION piProcInfo = {0};
    BOOL ok = FALSE;
    int applType;
    /* Not to be changed for different types of executables */
    int staticCreateFlags = GetPriorityClass(GetCurrentProcess()); 
    int createFlags = DETACHED_PROCESS;
    wchar_t *newcmdline = NULL;
    int cmdlength;
    wchar_t* thecommand;
    wchar_t* appname = NULL;
    HANDLE hProcess = GetCurrentProcess();
    STARTUPINFOW siStartInfo = {0};
    wchar_t execPath[MAX_PATH];

    *errno_return = -1;
    siStartInfo.cb = sizeof(STARTUPINFOW);
    siStartInfo.dwFlags = STARTF_USESTDHANDLES;
    siStartInfo.hStdInput = hStdin;
    siStartInfo.hStdOutput = hStdout;
    siStartInfo.hStdError = hStderr;

    if (st != ERTS_SPAWN_EXECUTABLE) {
	/*
	 * Parse out the program name from the command line (it can be quoted and
	 * contain spaces).
	 */
	cmdlength = parse_command(origcmd);
	newcmdline = (wchar_t *) erts_alloc(ERTS_ALC_T_TMP, (MAX_PATH+wcslen(origcmd)-cmdlength)*sizeof(wchar_t));
	thecommand = (wchar_t *) erts_alloc(ERTS_ALC_T_TMP, (cmdlength+1)*sizeof(wchar_t));
	wcsncpy(thecommand, origcmd, cmdlength);
	thecommand[cmdlength] = L'\0';
	DEBUGF(("spawn command: %S\n", thecommand));

	applType = application_type(thecommand, execPath, TRUE, TRUE, errno_return);
	DEBUGF(("application_type returned for (%S) is %d\n", thecommand, applType));
	erts_free(ERTS_ALC_T_TMP, (void *) thecommand);
	if (applType == APPL_NONE) {
	    erts_free(ERTS_ALC_T_TMP,newcmdline);
	    return FALSE;
	}
	newcmdline[0] = L'\0';

	if (applType == APPL_DOS) {
	    /*
	     * Under NT, 16-bit DOS applications will not run unless they
	     * can be attached to a console.  Run the 16-bit program as
	     * a normal process inside of a hidden console application,
	     * and then run that hidden console as a detached process.
	     */

	    siStartInfo.wShowWindow = SW_HIDE;
	    siStartInfo.dwFlags |= STARTF_USESHOWWINDOW;
	    createFlags = CREATE_NEW_CONSOLE;
	    wcscat(newcmdline, L"cmd.exe /c ");
	} else if (hide) {
	    DEBUGF(("hiding window\n"));
	    siStartInfo.wShowWindow = SW_HIDE;
	    siStartInfo.dwFlags |= STARTF_USESHOWWINDOW;
	    createFlags = 0;
	}

	wcscat(newcmdline, execPath);
	wcscat(newcmdline, origcmd+cmdlength);
	DEBUGF(("Creating child process: %S, createFlags = %d\n", newcmdline, createFlags));
	ok = CreateProcessW(appname,
			    newcmdline,
			    NULL,
			    NULL,
			    TRUE,
			    createFlags | staticCreateFlags |
			    CREATE_UNICODE_ENVIRONMENT,
			    env,
			    wd,
			    &siStartInfo,
			    &piProcInfo);

    } else { /* ERTS_SPAWN_EXECUTABLE, filename and args are in unicode ({utf16,little}) */
	int run_cmd = 0;

	applType = application_type(origcmd, execPath, FALSE, FALSE, errno_return);
	if (applType == APPL_NONE) {
	    return FALSE;
	} 
	if (applType == APPL_DOS) {
	    /*
	     * See comment above
	     */
		
	    siStartInfo.wShowWindow = SW_HIDE;
	    siStartInfo.dwFlags |= STARTF_USESHOWWINDOW;
	    createFlags = CREATE_NEW_CONSOLE;
	    run_cmd = 1;
	} else if (hide) {
	    DEBUGF(("hiding window\n"));
	    siStartInfo.wShowWindow = SW_HIDE;
	    siStartInfo.dwFlags |= STARTF_USESHOWWINDOW;
	    createFlags = 0;
	}
	if (run_cmd) {
	    wchar_t cmdPath[MAX_PATH];
	    int cmdType;
	    cmdType = application_type(L"cmd.exe", cmdPath, TRUE, FALSE, errno_return);
	    if (cmdType == APPL_NONE || cmdType == APPL_DOS) {
		return FALSE;
	    }
	    appname = (wchar_t *) erts_alloc(ERTS_ALC_T_TMP, (wcslen(cmdPath)+1)*sizeof(wchar_t));
	    wcscpy(appname,cmdPath);
	} else {
	    appname = (wchar_t *) erts_alloc(ERTS_ALC_T_TMP, (wcslen(execPath)+1)*sizeof(wchar_t));
	    wcscpy(appname, execPath);
	}
	if (argv == NULL) { 
	    BOOL orig_need_q;
	    wchar_t *ptr;
	    int ocl = escape_and_quote(execPath, NULL, &orig_need_q);
	    if (run_cmd) {
		newcmdline = (wchar_t *) erts_alloc(ERTS_ALC_T_TMP,
						    (ocl + 1 + 11)*sizeof(wchar_t));
		memcpy(newcmdline,L"cmd.exe /c ",11*sizeof(wchar_t));
		ptr = newcmdline + 11;
	    } else {
		newcmdline = (wchar_t *) erts_alloc(ERTS_ALC_T_TMP,
						    (ocl + 1)*sizeof(wchar_t));
		ptr = (wchar_t *) newcmdline;
	    }
	    ptr += escape_and_quote(execPath, ptr, &orig_need_q);
	    ptr[0] = L'\0';
	} else {
	    int sum = 0;
	    BOOL *qte = NULL;
	    wchar_t **ar = argv;
	    wchar_t *n;
	    wchar_t *save_arg0 = NULL;
	    if (argv[0] == (wchar_t *) erts_default_arg0 || run_cmd) {
		save_arg0 = argv[0];
		argv[0] = execPath;
	    }
	    if (run_cmd) {
		sum += 11; /* cmd.exe /c */
	    }

	    while (*ar != NULL)  ar++;
	    qte = erts_alloc(ERTS_ALC_T_TMP, (ar - argv)*sizeof(BOOL));

	    ar = argv;
	    while (*ar != NULL) {
		sum += escape_and_quote(*ar,NULL,qte+(ar - argv));
		sum++; /* space */
		++ar;
	    }
	    ar = argv;
	    newcmdline = (wchar_t *) erts_alloc(ERTS_ALC_T_TMP, sum*sizeof(wchar_t));
	    n = newcmdline;
	    if (run_cmd) {
		memcpy(n,L"cmd.exe /c ",11*sizeof(wchar_t));
		n += 11;
	    }
	    while (*ar != NULL) {
		n += escape_and_quote(*ar,n,qte+(ar - argv));
		*n++ = L' ';
		++ar;
	    }
	    *(n-1) = L'\0'; /* overwrite last space with '\0' */
	    if (save_arg0 != NULL) {
		argv[0] = save_arg0;
	    }
	    erts_free(ERTS_ALC_T_TMP, qte);
	}	    
	    
	DEBUGF((stderr,"Creating child process: %S, createFlags = %d\n", newcmdline, createFlags));
	ok = CreateProcessW((wchar_t *) appname,
			    (wchar_t *) newcmdline,
			    NULL,
			    NULL,
			    TRUE,
			    createFlags | staticCreateFlags |
			    CREATE_UNICODE_ENVIRONMENT,
			    env,
			    wd,
			    &siStartInfo,
			    &piProcInfo);

    } /* end SPAWN_EXECUTABLE */
    if (newcmdline != NULL) {
	    erts_free(ERTS_ALC_T_TMP,newcmdline);
    }	
    if (appname != NULL) {
	    erts_free(ERTS_ALC_T_TMP,appname);
    }	
    if (!ok) {
	DEBUGF(("CreateProcess failed: %s\n", last_error()));
	if (*errno_return < 0) {
	    *errno_return = EACCES;
	}
	return FALSE;
    }
    CloseHandle(piProcInfo.hThread); /* Necessary to avoid resource leak. */
    *phPid = piProcInfo.hProcess;
    *pdwID = piProcInfo.dwProcessId;

    if (applType == APPL_DOS) {
	WaitForSingleObject(hProcess, 50);
    }
    
    return ok;
}

/* 
 * Note, inheritRead == FALSE means "inhetitWrite", i e one of the
 * pipe ends is always expected to be inherited. The pipe end that should 
 * be inherited is opened without overlapped io flags, as the child program
 * would expect stdout not to demand overlapped I/O. 
 */
static int create_pipe(HANDLE *phRead, HANDLE *phWrite, BOOL inheritRead, BOOL overlapped_io)
{
    SECURITY_ATTRIBUTES sa = {sizeof(SECURITY_ATTRIBUTES), NULL, TRUE};
    char pipe_name[256];	/* Name of pipe. */
    Uint calls;

    /*
     * If we should't use named pipes, create anonmous pipes.
     */

    if (!use_named_pipes) {
	int success;
	HANDLE non_inherited;	/* Non-inherited copy of handle. */

	if (!CreatePipe(phRead, phWrite, &sa, 0)) {
	    DEBUGF(("Error creating anonyomous pipe: %s\n", last_error()));
	    return FALSE;
	}

	if (inheritRead) {
	    success = DuplicateHandle(GetCurrentProcess(), *phWrite,
				      GetCurrentProcess(), &non_inherited, 0,
				      FALSE, DUPLICATE_SAME_ACCESS);
	    CloseHandle(*phWrite);
	    *phWrite = non_inherited;
	} else {
	    success = DuplicateHandle(GetCurrentProcess(), *phRead,
				      GetCurrentProcess(), &non_inherited, 0,
				      FALSE, DUPLICATE_SAME_ACCESS);
	    CloseHandle(*phRead);
	    *phRead = non_inherited;
	}
	return success;
    }


    /*
     * Otherwise, create named pipes.
     */

    calls = (UWord) erts_atomic_inc_read_nob(&pipe_creation_counter);
    erts_snprintf(pipe_name, sizeof(pipe_name),
		  "\\\\.\\pipe\\erlang44_%d_%bpu", getpid(), calls);

    DEBUGF(("Creating pipe %s\n", pipe_name));
    sa.bInheritHandle = inheritRead;
    if ((*phRead = CreateNamedPipe(pipe_name,
				   PIPE_ACCESS_INBOUND | 
				   ((inheritRead && !overlapped_io) ? 0 : FILE_FLAG_OVERLAPPED),
				   PIPE_TYPE_BYTE | PIPE_READMODE_BYTE,
				   1,
				   0,
				   0,
				   2000,
				   &sa)) == NULL) {
	DEBUGF(("Error creating pipe: %s\n", last_error()));
	return FALSE;
    }
  
    sa.bInheritHandle = !inheritRead;
    if ((*phWrite = CreateFile(pipe_name,
			       GENERIC_WRITE,
			       0, /* No sharing */
			       &sa,
			       OPEN_EXISTING,
			       FILE_ATTRIBUTE_NORMAL | 
			       ((inheritRead || overlapped_io) ? FILE_FLAG_OVERLAPPED : 0),
			       NULL)) == INVALID_HANDLE_VALUE) {
	CloseHandle(*phRead);
	DEBUGF(("Error opening other end of pipe: %s\n", last_error()));
	return FALSE;
    }
    return TRUE;
}

static int application_type (const wchar_t *originalName, /* Name of the application to find. */
			     wchar_t wfullpath[MAX_PATH],/* Filled with complete path to
							  * application. */
			     BOOL search_in_path,      /* If we should search the system wide path */
			     BOOL handle_quotes,       /* If we should handle quotes around executable */
			     int *error_return)         /* A place to put an error code */
{
    int applType, i;
    HANDLE hFile;
    wchar_t *ext, *rest;
    char buf[2];
    DWORD read;
    IMAGE_DOS_HEADER header;
    static wchar_t extensions[][5] = {L"", L".com", L".exe", L".bat"};
    int is_quoted;
    int len;
    wchar_t xfullpath[MAX_PATH];

    len = wcslen(originalName);
    is_quoted = handle_quotes && len > 0 && originalName[0] == L'"' && 
	originalName[len-1] == L'"';

    applType = APPL_NONE;
    *error_return = ENOENT;
    for (i = 0; i < (int) (sizeof(extensions) / sizeof(extensions[0])); i++) {
	if(is_quoted) {
	   lstrcpynW(xfullpath, originalName+1, MAX_PATH - 7); /* Cannot start using StringCchCopy yet, we support
							   older platforms */
	   len = wcslen(xfullpath);
	   if(len > 0) {
	       xfullpath[len-1] = L'\0';
	   }
	} else {
	    lstrcpynW(xfullpath, originalName, MAX_PATH - 5);
	}
	wcscat(xfullpath, extensions[i]);
	/* It seems that the Unicode version does not allow in and out parameter to overlap. */
	SearchPathW((search_in_path) ? NULL : L".", xfullpath, NULL, MAX_PATH, wfullpath, &rest);

	/*
	 * Ignore matches on directories or data files, return if identified
	 * a known type.
	 */

	if (GetFileAttributesW(wfullpath) & FILE_ATTRIBUTE_DIRECTORY) {
	    continue;
	}

	ext = wcsrchr(wfullpath, L'.');
	if ((ext != NULL) && (_wcsicmp(ext, L".bat") == 0)) {
	    *error_return = EACCES;
	    applType = APPL_DOS;
	    break;
	}

	hFile = CreateFileW(wfullpath, GENERIC_READ, FILE_SHARE_READ, NULL, 
		OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
	if (hFile == INVALID_HANDLE_VALUE) {
	    continue;
	}

	*error_return = EACCES; /* If considered an error, 
				    it's an access error */
	header.e_magic = 0;
	ReadFile(hFile, (void *) &header, sizeof(header), &read, NULL);
	if (header.e_magic != IMAGE_DOS_SIGNATURE) {
	    /* 
	     * Doesn't have the magic number for relocatable executables.  If 
	     * filename ends with .com, assume it's a DOS application anyhow.
	     * Note that we didn't make this assumption at first, because some
	     * supposed .com files are really 32-bit executables with all the
	     * magic numbers and everything.  
	     */

	    CloseHandle(hFile);
	    if ((ext != NULL) && (_wcsicmp(ext, L".com") == 0)) {
		applType = APPL_DOS;
		break;
	    }
	    continue;
	}
	if (header.e_lfarlc != sizeof(header)) {
	    /* 
	     * All Windows 3.X and Win32 and some DOS programs have this value
	     * set here.  If it doesn't, assume that since it already had the 
	     * other magic number it was a DOS application.
	     */

	    CloseHandle(hFile);
	    applType = APPL_DOS;
	    break;
	}

	/* 
	 * The DWORD at header.e_lfanew points to yet another magic number.
	 */

	buf[0] = '\0';
	SetFilePointer(hFile, header.e_lfanew, NULL, FILE_BEGIN);
	ReadFile(hFile, (void *) buf, 2, &read, NULL);
	CloseHandle(hFile);

	if ((buf[0] == 'L') && (buf[1] == 'E')) {
	    applType = APPL_DOS;
	} else if ((buf[0] == 'N') && (buf[1] == 'E')) {
	    applType = APPL_WIN3X;
	} else if ((buf[0] == 'P') && (buf[1] == 'E')) {
	    applType = APPL_WIN32;
	} else {
	    continue;
	}
	break;
    }

    if (applType == APPL_NONE) {
	return APPL_NONE;
    }

    if ((applType == APPL_DOS) || (applType == APPL_WIN3X)) {
	/* 
	 * Replace long path name of executable with short path name for 
	 * 16-bit applications.  Otherwise the application may not be able
	 * to correctly parse its own command line to separate off the 
	 * application name from the arguments.
	 */

	GetShortPathNameW(wfullpath, wfullpath, MAX_PATH);
    }
    if (is_quoted) {
	/* restore quotes on quoted program name */
	len = wcslen(wfullpath);
	memmove(wfullpath+1,wfullpath,len*sizeof(wchar_t));
	wfullpath[0]=L'"';
	wfullpath[len+1]=L'"';
	wfullpath[len+2]=L'\0';
    }
    return applType;
}

/*
 * Thread function used to emulate overlapped reading.
 */

DWORD WINAPI
threaded_reader(LPVOID param)
{
    AsyncIo* aio = (AsyncIo *) param;
    HANDLE thread = GetCurrentThread();
    char* buf;
    DWORD numToRead;
  
    for (;;) {
	WaitForSingleObject(aio->ioAllowed, INFINITE);
	if (aio->flags & DF_EXIT_THREAD)
	    break;
	buf = OV_BUFFER_PTR(aio);
	numToRead = OV_NUM_TO_READ(aio);
	aio->pendingError = 0;
	if (!ReadFile(aio->fd, buf, numToRead, &aio->bytesTransferred, NULL)) {
	    int error = GetLastError();
	    aio->pendingError = error;
	} else if (aio->flags & DF_XLAT_CR) {
	    char *s;
	    int n;
	    
	    n = aio->bytesTransferred;
	    for (s = buf; s < buf+n; s++) {
		if (*s == '\r') {
		    if (s < buf + n - 1 && s[1] == '\n') {
			memmove(s, s+1, (buf+n - s - 1));
			--n;
		    } else {
			*s = '\n';
		    }
		}
	    }
	    aio->bytesTransferred = n;
	}
	SetEvent(aio->ov.hEvent);
	if ((aio->flags & DF_XLAT_CR) == 0 && aio->bytesTransferred == 0) {
	    break;
	}
	if (aio->pendingError != NO_ERROR) {
	    break;
	}
	if (aio->flags & DF_EXIT_THREAD)
	    break;
    }
    unrefer_driver_data(aio->dp);
    return 0;
}

/*
 * Thread function used to emulate overlapped writing
 */

DWORD WINAPI
threaded_writer(LPVOID param)
{
    AsyncIo* aio = (AsyncIo *) param;
    HANDLE thread = GetCurrentThread();
    char* buf;
    DWORD numToWrite, handle;
    int ok;
    HANDLE handles[2];
    handles[0] = aio->ioAllowed;
    handles[1] = aio->flushEvent;
  
    for (;;) {
	handle = WaitForMultipleObjects(2, handles, FALSE, INFINITE);
	if (aio->flags & DF_EXIT_THREAD) {
	    break;
	}

	buf = OV_BUFFER_PTR(aio);
	numToWrite = OV_NUM_TO_READ(aio);
	aio->pendingError = 0;

	if (handle == (WAIT_OBJECT_0 + 1) && numToWrite == 0) {
	  SetEvent(aio->flushReplyEvent);
	  aio->flags |= DF_THREAD_FLUSHED;
	  continue;
	}

	ok = WriteFile(aio->fd, buf, numToWrite, &aio->bytesTransferred, NULL);
	if (!ok) {
	    aio->pendingError = GetLastError();
	    if (aio->pendingError == ERROR_INVALID_HANDLE && 
		aio->flags & DF_DROP_IF_INVH) {
		/* This is standard error and we'we got an 
		   invalid standard error FD (non-inheritable) from parent. 
		   Just drop the message and be happy. */
		aio->pendingError = 0;
		aio->bytesTransferred = numToWrite;
	    } else if (aio->pendingError == ERROR_NOT_ENOUGH_MEMORY) {
		/* This could be a console, which limits utput to 64kbytes, 
		   which might translate to less on a unicode system. 
		   Try 16k chunks and see if it works before giving up. */
		int done = 0;
		DWORD transferred;
		aio->pendingError = 0;
		aio->bytesTransferred = 0;
		ok = 1;
		while (ok && (numToWrite - done) > 0x4000) {
		    ok = WriteFile(aio->fd, buf + done, 0x4000, &transferred, NULL);
		    aio->bytesTransferred += transferred;
		    done += 0x4000;
		}
		if (ok && (numToWrite - done) > 0) {
		    ok = WriteFile(aio->fd, buf + done, (numToWrite - done), 
				   &transferred, NULL);
		    aio->bytesTransferred += transferred;
		}
		if (!ok) {
		    aio->pendingError = GetLastError();
		}  
	    }
	}
	OV_NUM_TO_READ(aio) = 0;
	if (handle == (WAIT_OBJECT_0 + 1))
	    SetEvent(aio->flushReplyEvent);
	else
	    SetEvent(aio->ov.hEvent);
	if (aio->pendingError != NO_ERROR || aio->bytesTransferred == 0)
	    break;
	if (aio->flags & DF_EXIT_THREAD)
	    break;
    }
    aio->flags |= DF_THREAD_FLUSHED;
    CloseHandle(aio->fd);
    aio->fd = INVALID_HANDLE_VALUE;
    unrefer_driver_data(aio->dp);
    return 0;
}

static HANDLE
translate_fd(int fd)
{
    DWORD access;
    HANDLE handle;

    switch (fd) {
    case 0:
	access = GENERIC_READ;
	handle = GetStdHandle(STD_INPUT_HANDLE);
	break;
    case 1:
	access = GENERIC_WRITE;
	handle = GetStdHandle(STD_OUTPUT_HANDLE);
	break;
    case 2:
	access = GENERIC_WRITE;
	handle = GetStdHandle(STD_ERROR_HANDLE);
	break;
    default:
	return (HANDLE) fd;
    }
    DEBUGF(("translate_fd(%d) -> std(%d)\n", fd, handle));

    if (handle == INVALID_HANDLE_VALUE || handle == 0) {
	handle = CreateFile("nul", access, 0,
			    NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    }
    DEBUGF(("translate_fd(%d) -> %d\n", fd, handle));
    return handle;
}

/* Driver level locking, start function is serialized */
static DriverData *save_01_port = NULL;
static DriverData *save_22_port = NULL;

static ErlDrvData
fd_start(ErlDrvPort port_num, char* name, SysDriverOpts* opts)
{
    DriverData* dp;
    int is_std_error = (opts->ofd == 2);
    int in = opts->ifd, out = opts->ofd;

    opts->ifd = (Uint) translate_fd(in);
    opts->ofd = (Uint) translate_fd(out);
    if ( in == 0 && out == 1 && save_01_port != NULL) {
	dp = save_01_port;
	return reuse_driver_data(dp, (HANDLE) opts->ifd, (HANDLE) opts->ofd, opts->read_write, port_num);
    } else if (in == 2 && out == 2 && save_22_port != NULL) {
	dp = save_22_port;
	return reuse_driver_data(dp, (HANDLE) opts->ifd, (HANDLE) opts->ofd, opts->read_write, port_num);
    } else {
	if ((dp = new_driver_data(port_num, opts->packet_bytes, 2, TRUE)) == NULL)
	    return ERL_DRV_ERROR_GENERAL;
	
	/**
	 * Here is a brief description about how the fd driver works on windows.
	 *
	 * fd_init:
	 * For each in/out fd pair a threaded_reader and threaded_writer thread is
	 * created. Within the DriverData struct each of the threads have an AsyncIO
	 * sctruct associated with it.  Within AsyncIO there are two important HANDLEs,
	 * ioAllowed and ov.hEvent. ioAllowed is used to signal the threaded_* threads
	 * should read/write some data, and ov.hEvent is driver_select'ed to be used to
	 * signal that the thread is done reading/writing.
	 *
	 * The reason for the driver being threaded like this is because once the FD is open
	 * on windows, it is not possible to set the it in overlapped mode. So we have to
	 * simulate this using threads.
	 *
	 * output:
	 * When an output occurs the data to be outputted is copied to AsyncIO.ov. Then
	 * the ioAllowed HANDLE is set, ov.hEvent is cleared and the port is marked as busy.
	 * The threaded_writer thread is lying in WaitForMultipleObjects on ioAllowed, and
	 * when signalled it writes all data in AsyncIO.ov and then sets ov.hEvent so that
	 * ready_output gets triggered and (potentially) sends the reply to the port and
	 * marks the port an non-busy.
	 *
	 * input:
	 * The threaded_reader is lying waiting in ReadFile on the in fd and when a new
	 * line is written it sets ov.hEvent that new data is available and then goes
	 * and waits for ioAllowed to be set. ready_input is run when ov.hEvent is set and
	 * delivers the data to the port. Then ioAllowed is signalled again and threaded_reader
	 * goes back to ReadFile.
	 *
	 * shutdown:
	 * In order to guarantee that all io is outputted before the driver is stopped,
	 * fd_stop uses flushEvent and flushReplyEvent to make sure that there is no data
	 * in ov which needs writing before returning from fd_stop.
	 *
	 **/

	if (!create_file_thread(&dp->in, DO_READ)) {
	    return ERL_DRV_ERROR_GENERAL;
	}
	
	if (!create_file_thread(&dp->out, DO_WRITE)) {
	    return ERL_DRV_ERROR_GENERAL;
	}
	
	fd_driver_input = &(dp->in);
	dp->in.flags = DF_XLAT_CR;
	if (is_std_error) {
	    dp->out.flags |= DF_DROP_IF_INVH; /* Just drop messages if stderror
						 is an invalid handle */
	}
	
	if ( in == 0 && out == 1) {
	    save_01_port = dp;
	} else if (in == 2 && out == 2) {
	    save_22_port = dp;
	}
	return set_driver_data(dp, (HANDLE) opts->ifd, (HANDLE) opts->ofd, opts->read_write, 0);
    }
}

static void fd_stop(ErlDrvData data)
{
  DriverData * dp = (DriverData *) data;
  /*
   * There's no way we can terminate an fd port in a consistent way.
   * Instead we let it live until it's opened again (which it is,
   * as the only FD-drivers are for 0,1 and 2 adn the only time they
   * get closed is by init:reboot).
   * So - just deselect them and let everything be as is. 
   * They get woken up in fd_start again, where the DriverData is
   * remembered. /PaN
   */
  if (dp->in.ov.hEvent != NULL) {
      (void) driver_select(dp->port_num,
			   (ErlDrvEvent)dp->in.ov.hEvent,
			   ERL_DRV_READ, 0);
  }
  if (dp->out.ov.hEvent != NULL) {
      (void) driver_select(dp->port_num,
			   (ErlDrvEvent)dp->out.ov.hEvent,
			   ERL_DRV_WRITE, 0);
      do {
	ASSERT(dp->out.flushEvent);
	SetEvent(dp->out.flushEvent);
      } while (WaitForSingleObject(dp->out.flushReplyEvent, 10) == WAIT_TIMEOUT
	       && !(dp->out.flags & DF_THREAD_FLUSHED));
  }    

}

static ErlDrvData
vanilla_start(ErlDrvPort port_num, char* name, SysDriverOpts* opts)
{
    HANDLE ofd,ifd;
    DriverData* dp;
    DWORD access;		/* Access mode: GENERIC_READ, GENERIC_WRITE. */
    DWORD crFlags;
    HANDLE this_process = GetCurrentProcess();

    access = 0;
    if (opts->read_write == DO_READ)
	access |= GENERIC_READ;
    if (opts->read_write == DO_WRITE)
	access |= GENERIC_WRITE;

    if (opts->read_write == DO_READ)
	crFlags = OPEN_EXISTING;
    else if (opts->read_write == DO_WRITE)
	crFlags = CREATE_ALWAYS;
    else
	crFlags = OPEN_ALWAYS;

    if ((dp = new_driver_data(port_num, opts->packet_bytes, 2, FALSE)) == NULL)
	return ERL_DRV_ERROR_GENERAL;
    ofd = CreateFile(name, access, FILE_SHARE_READ | FILE_SHARE_WRITE,
		    NULL, crFlags, FILE_ATTRIBUTE_NORMAL, NULL);
    if (!DuplicateHandle(this_process, (HANDLE) ofd,	
			 this_process, &ifd, 0,
			 FALSE, DUPLICATE_SAME_ACCESS)) {
	CloseHandle(ofd);
	ofd = INVALID_HANDLE_VALUE;
    }
    if (ofd == INVALID_HANDLE_VALUE)
	return ERL_DRV_ERROR_GENERAL;
    return set_driver_data(dp, ifd, ofd, opts->read_write,0);
}

static void
stop(ErlDrvData data)
{
    DriverData *dp = (DriverData *) data;
    DEBUGF(("stop(%p)\n", dp));

    if (dp->in.ov.hEvent != NULL) {
	(void) driver_select(dp->port_num,
			     (ErlDrvEvent)dp->in.ov.hEvent,
			     ERL_DRV_READ|ERL_DRV_USE_NO_CALLBACK, 0);
    }
    if (dp->out.ov.hEvent != NULL) {
	(void) driver_select(dp->port_num,
			     (ErlDrvEvent)dp->out.ov.hEvent,
			     ERL_DRV_WRITE|ERL_DRV_USE_NO_CALLBACK, 0);
    }    

    if (dp->out.thread == (HANDLE) -1 && dp->in.thread == (HANDLE) -1) {
	release_driver_data(dp);
    } else {
	/*
	 * If there are read or write threads, start a thread which will
	 * wait for them to finish.
	 */
	HANDLE thread;
	DWORD tid;

	/* threaded_exiter implicitly takes over refc from us... */
	thread = (HANDLE *) _beginthreadex(NULL, 0, threaded_exiter, dp, 0, &tid);
	CloseHandle(thread);
    }
}

DWORD WINAPI
threaded_exiter(LPVOID param)
{
    DriverData* dp = (DriverData *) param;
    HANDLE handles[2];
    int i;

    /*
     * Ask the threads to terminated.
     *
     * Note that we can't reliable test the state of the ioAllowed event,
     * because it is an auto reset event.  Therefore, always set the
     * exit flag and signal the event.
     */
    i = 0;
    if (dp->out.thread != (HANDLE) -1) {
	dp->out.flags |= DF_EXIT_THREAD;
	SetEvent(dp->out.ioAllowed);
	handles[i++] = dp->out.thread;
    }
    if (dp->in.thread != (HANDLE) -1) {
	dp->in.flags |= DF_EXIT_THREAD;
	SetEvent(dp->in.ioAllowed);
	handles[i++] = dp->in.thread;
    }

    /*
     * If we were lucky, the following happened above:
     * 	1) The output thread terminated (and closed the pipe).
     *  2) As a consequence of that, the port program received
     *     EOF on its standard input.
     *  3) Hopefully, because of (2), the port program terminated.
     *  4) Because of (3), the input thread terminated.
     *
     * But this might need some time; therefore, we must wait for
     * both threads to terminate.
     */

    if (i > 0) {
	switch (WaitForMultipleObjects(i, handles, TRUE, 5000)) {
	case WAIT_TIMEOUT:
	    DEBUGF(("Timeout waiting for %d threads failed\n", i));
	    break;
	case WAIT_FAILED:
	    DEBUGF(("Wait for %d threads failed: %s\n",
		    i, win32_errorstr(GetLastError())));
	    break;
	default:
	    break;
	}
    }

    /*
     * Wait for threads to terminate didn't help.  Now use some force.
     * TerminateThread() is *not* a good idea, because it doesn't clean
     * up the thread's stack.
     *
     * Instead we well terminate the port program and wait for the
     * threads to terminate themselves when they receive end of file.
     */

    if (dp->out.thread != (HANDLE) -1) {
	int error;

	if (WaitForSingleObject(dp->out.thread, 0) == WAIT_OBJECT_0) {
	    CloseHandle(dp->out.thread);
	    dp->out.thread = (HANDLE) -1;
	} else if (dp->port_pid != INVALID_HANDLE_VALUE) {
	    DEBUGF(("Killing port process 0x%x (output thread)\n", dp->port_pid));
	    TerminateProcess(dp->port_pid, 0);
	    if (!CloseHandle(dp->port_pid))
	        DEBUGF(("Failed to close output handle!!!\n"));
	    dp->port_pid = INVALID_HANDLE_VALUE;
	    DEBUGF(("Waiting for output thread 0x%x to finish\n", dp->out.thread));
	    error = WaitForSingleObject(dp->out.thread, INFINITE);
	}
    }
    
    if (dp->in.thread != (HANDLE) -1) {
	if (WaitForSingleObject(dp->in.thread, 0) == WAIT_OBJECT_0) {
	    CloseHandle(dp->in.thread);
	    dp->in.thread = (HANDLE) -1;
	} else if (dp->port_pid != INVALID_HANDLE_VALUE) {
	    DEBUGF(("Killing port process 0x%x (input thread)\n", dp->port_pid));
	    TerminateProcess(dp->port_pid, 0);
	    if (!CloseHandle(dp->port_pid))
	        DEBUGF(("Failed to close input handle!!!\n"));
	    dp->port_pid = INVALID_HANDLE_VALUE;

	    DEBUGF(("Waiting for input thread 0x%x to finish\n", dp->in.thread));
	    switch (WaitForSingleObject(dp->in.thread, INFINITE)) {
	    case WAIT_OBJECT_0:
		CloseHandle(dp->in.thread);
		dp->in.thread = (HANDLE) -1;
		break;
	    default:
		DEBUGF(("Wait for input thread to finish failed: %s\n",
			win32_errorstr(GetLastError())));
		break;
	    }
	}
    }

    release_driver_data(dp);
    return 0;
}

/* ----------------------------------------------------------------------
 * output --
 * 	Outputs data from Erlang to the port program.
 *
 * Results:
 *	Returns the actual number of bytes written (including the
 *	packet header) or -1 if an error occurred.
 * ----------------------------------------------------------------------
 */

static void
output(ErlDrvData drv_data, char* buf, ErlDrvSizeT len)
/*     ErlDrvData drv_data;	/* The slot to use in the driver data table.
				 * For Windows NT, this is *NOT* a file handle.
				 * The handle is found in the driver data.
				 */
/*     char *buf;		/* Pointer to data to write to the port program. */
/*     ErlDrvSizeT len;		/* Number of bytes to write. */
{
    DriverData* dp = (DriverData *) drv_data;
    int pb;			/* The header size for this port. */
    char* current;

    pb = dp->packet_bytes;

    if ((pb+len) == 0)
	return ; /* 0; */

    /*
     * Check that the message can be sent with given header length.
     */

    if ((pb == 2 && len > 65535) || (pb == 1 && len > 255)) {
	driver_failure_posix(dp->port_num, EINVAL);
	return ; /* -1; */
    }

    /*
     * Allocate memory for both the message and the header.
     */

    ASSERT(dp->outbuf == NULL);
    ASSERT(dp->outBufSize == 0);

    ASSERT(!dp->outbuf);
    dp->outbuf = DRV_BUF_ALLOC(pb+len);
    if (!dp->outbuf) {
	driver_failure_posix(dp->port_num, ENOMEM);
	return ; /* -1; */
    }

    dp->outBufSize = pb+len;
    erts_atomic_add_nob(&sys_misc_mem_sz, dp->outBufSize);

    /*
     * Store header bytes (if any).
     */

    current = dp->outbuf;
    switch (pb) {
    case 4:
	*current++ = (len >> 24) & 255;
	*current++ = (len >> 16) & 255;
    case 2:
	*current++ = (len >> 8) & 255;
    case 1:
	*current++ = len & 255;
    }

    /*
     * Start the write.
     */

    if (len)
	memcpy(current, buf, len);
    
    if (!async_write_file(&dp->out, dp->outbuf, pb+len)) {
	set_busy_port(dp->port_num, 1);
    } else {
	dp->out.ov.Offset += pb+len; /* For vanilla driver. */
	/* XXX OffsetHigh should be changed too. */
	ASSERT(erts_atomic_read_nob(&sys_misc_mem_sz) >= dp->outBufSize);
	erts_atomic_add_nob(&sys_misc_mem_sz, -1*dp->outBufSize);
	DRV_BUF_FREE(dp->outbuf);
	dp->outBufSize = 0;
	dp->outbuf = NULL;
    }
    /*return 0;*/
}


/* ----------------------------------------------------------------------
 * ready_input --
 *	This function is called (indirectly) from check_io() when an
 *	event object has been signaled, indicating that there is
 *	something to read on the corresponding file handle.
 *
 *	If the port is working in the continuous stream mode (packet_bytes == 0),
 *	whatever data read will be sent straight to Erlang.
 *
 * Results:
 *	Always 0.
 * ----------------------------------------------------------------------
 */

static void
ready_input(ErlDrvData drv_data, ErlDrvEvent ready_event)
/*     long drv_data;		/* Driver data. */
/*     HANDLE ready_event;	/* The handle for the ready event. */
{
    int error = 0;		/* The error code (assume initially no errors). */
    DWORD bytesRead;		/* Number of bytes read. */
    DriverData* dp = (DriverData *) drv_data;
    int pb;

    pb = dp->packet_bytes;
    if(dp->in.thread == (HANDLE) -1) {
	dp->in.async_io_active = 0;
    }
    DEBUGF(("ready_input: dp %p, event 0x%x\n", dp, ready_event));

    /*
     * Evaluate the result of the overlapped read.
     */

#ifdef HARD_POLL_DEBUG
     poll_debug_read_begin(dp->in.ov.hEvent);
#endif

    error = get_overlapped_result(&dp->in, &bytesRead, TRUE);

#ifdef HARD_POLL_DEBUG
    poll_debug_read_done(dp->in.ov.hEvent,bytesRead);
#endif

    if (error == NO_ERROR) {
	if (pb == 0) { /* Continuous stream. */
#ifdef DEBUG
	    DEBUGF(("ready_input: %d: ", bytesRead));
	    erl_bin_write(dp->inbuf, 16, bytesRead);
	    DEBUGF(("\n"));
#endif
	    driver_output(dp->port_num, dp->inbuf, bytesRead);
	} else {			/* Packet mode */
	    dp->bytesInBuffer += bytesRead;

	    /*
	     * Loop until we've exhausted the data in the buffer.
	     */

	    for (;;) {

		/*
		 * Check for completion of a header read.
		 */

		if (dp->bytesInBuffer >= dp->totalNeeded &&
		    dp->totalNeeded == pb) {

		    /*
		     * We have successfully read the packet header
		     * (and perhaps even the packet).  Get the packet size
		     * from the header and update dp->totalNeeded to include
		     * the packet size.
		     */

		    int packet_size = 0;
		    unsigned char *header = (unsigned char *) dp->inbuf;
		    
		    switch (pb) {
		    case 4:
			packet_size = (packet_size << 8) | *header++;
			packet_size = (packet_size << 8) | *header++;
		    case 2:
			packet_size = (packet_size << 8) | *header++;
		    case 1:
			packet_size = (packet_size << 8) | *header++;
		    }
		    
		    dp->totalNeeded += packet_size;
		    
		    /*
		     * Make sure that the receive buffer is big enough.
		     */
		    
		    if (dp->inBufSize < dp->totalNeeded) {
			char* new_buf;
		    
			new_buf = DRV_BUF_REALLOC(dp->inbuf, dp->totalNeeded);
			if (new_buf == NULL) {
			    error = ERROR_NOT_ENOUGH_MEMORY;
			    break; /* Break out of loop into error handler. */
			}
			ASSERT(erts_atomic_read_nob(&sys_misc_mem_sz) >= dp->inBufSize);
			erts_atomic_add_nob(&sys_misc_mem_sz,
						dp->totalNeeded - dp->inBufSize);
			dp->inBufSize = dp->totalNeeded;
			dp->inbuf = new_buf;
		    }
		}
		
		/*
		 * Check for completion of a packet read.
		 */
		
		if (dp->bytesInBuffer < dp->totalNeeded) {
		    /*
		     * Not enough bytes in the buffer.  Break out of
		     * the loop and initiate a new read.
		     */

		    break;
		} else {
		    
		    /*
		     * We have successfully read a complete packet, which
		     * can be passed to Erlang.
		     */
		    
		    driver_output(dp->port_num, dp->inbuf+pb, dp->totalNeeded-pb);
		    
		    /*
		     * Update the number of bytes remaining in the buffer,
		     * and move the data remaining (if any) to the beginning
		     * of the buffer.
		     */
		    
		    dp->bytesInBuffer -= dp->totalNeeded;
		    if (dp->bytesInBuffer > 0) {
			memmove(dp->inbuf, dp->inbuf+dp->totalNeeded,
				dp->bytesInBuffer);
		    }
		    
		    /*
		     * Indicate that we need the size of a header, and
		     * go through the loop once more (to either process
		     * remaining bytes or initiate reading more).
		     */
		    
		    dp->totalNeeded = pb;
		}
	    }
	}
    }

    /*
     * Start a new overlapped read, or report the error.
     */

    if (error == NO_ERROR) {
	async_read_file(&dp->in, dp->inbuf+dp->bytesInBuffer,
			dp->inBufSize - dp->bytesInBuffer);
    } else {
	DEBUGF(("ready_input(): error: %s\n", win32_errorstr(error)));
	if (error == ERROR_BROKEN_PIPE || error == ERROR_HANDLE_EOF) {
	    /* Maybe check exit status */
	    if (dp->report_exit) {
		DWORD exitcode;
		if (GetExitCodeProcess(dp->port_pid, &exitcode) &&
		    exitcode != STILL_ACTIVE) {
		    driver_report_exit(dp->port_num, exitcode);
		}
	    }
	    driver_failure_eof(dp->port_num);
	} else {			/* Report real errors. */
	    int error = GetLastError();

	    (void) driver_select(dp->port_num, ready_event, ERL_DRV_READ, 0);
	    _dosmaperr(error);
	    driver_failure_posix(dp->port_num, errno);
	}
    }

    /*return 0;*/
}

static void
ready_output(ErlDrvData drv_data, ErlDrvEvent ready_event)
{
    DWORD bytesWritten;
    DriverData *dp = (DriverData *) drv_data;
    int error;

    if(dp->out.thread == (HANDLE) -1) {
	dp->out.async_io_active = 0;
    }
    DEBUGF(("ready_output(%p, 0x%x)\n", drv_data, ready_event));
    set_busy_port(dp->port_num, 0);
    if (!(dp->outbuf)) {
	/* Happens because event sometimes get signalled during a successful
	   write... */
	return;
    }
    ASSERT(erts_atomic_read_nob(&sys_misc_mem_sz) >= dp->outBufSize);
    erts_atomic_add_nob(&sys_misc_mem_sz, -1*dp->outBufSize);
    DRV_BUF_FREE(dp->outbuf);
    dp->outBufSize = 0;
    dp->outbuf = NULL;
#ifdef HARD_POLL_DEBUG
    poll_debug_write_begin(dp->out.ov.hEvent);
#endif
    error = get_overlapped_result(&dp->out, &bytesWritten, TRUE);
#ifdef HARD_POLL_DEBUG
    poll_debug_write_done(dp->out.ov.hEvent,bytesWritten);
#endif

    if (error == NO_ERROR) {
	dp->out.ov.Offset += bytesWritten; /* For vanilla driver. */
	return ; /* 0; */
    }

    (void) driver_select(dp->port_num, ready_event, ERL_DRV_WRITE, 0);
    _dosmaperr(error);
    driver_failure_posix(dp->port_num, errno);
    /* return 0; */
}

static void stop_select(ErlDrvEvent e, void* _)
{
    CloseHandle((HANDLE)e);
}

/* Fills in the systems representation of the beam process identifier.
** The Pid is put in STRING representation in the supplied buffer,
** no interpretation of this should be done by the rest of the
** emulator. The buffer should be at least 21 bytes long.
*/
void sys_get_pid(char *buffer, size_t buffer_size){
    DWORD p = GetCurrentProcessId();
    /* The pid is scalar and is an unsigned long. */
    erts_snprintf(buffer, buffer_size, "%lu",(unsigned long) p);
}

void
sys_init_io(void)
{
    
    /* Now heres an icky one... This is called before drivers are, so we
       can change our view of the number of open files possible.
       We estimate the number to twice the amount of ports. 
       We really dont know on windows, do we? */
    max_files = 2*erts_ptab_max(&erts_port);
}

void
erts_sys_main_thread(void)
{
    HANDLE dummy;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_set_thread_name("parent_thread");
#endif
    dummy = CreateEvent(NULL, FALSE, FALSE, NULL);
    for(;;) {
	WaitForSingleObject(dummy, INFINITE);
    }
}

void erts_sys_alloc_init(void)
{
}

void *erts_sys_alloc(ErtsAlcType_t t, void *x, Uint sz)
{
    return malloc((size_t) sz);
}

void *erts_sys_realloc(ErtsAlcType_t t, void *x, void *p, Uint sz)
{
    return realloc(p, (size_t) sz);
}

void erts_sys_free(ErtsAlcType_t t, void *x, void *p)
{
    free(p);
}

void *erts_sys_aligned_alloc(UWord alignment, UWord size)
{
    void *ptr;
    ASSERT(alignment && (alignment & (alignment-1)) == 0); /* power of 2 */
    ptr = _aligned_malloc((size_t) size, (size_t) alignment);
    ASSERT(!ptr || (((UWord) ptr) & (alignment - 1)) == 0);
    return ptr;
}

void erts_sys_aligned_free(UWord alignment, void *ptr)
{
    ASSERT(alignment && (alignment & (alignment-1)) == 0); /* power of 2 */
    _aligned_free(ptr);
}

void *erts_sys_aligned_realloc(UWord alignment, void *ptr, UWord size, UWord old_size)
{
    void *new_ptr;
    ASSERT(alignment && (alignment & (alignment-1)) == 0); /* power of 2 */
    new_ptr = _aligned_realloc(ptr, (size_t) size, (size_t) alignment);
    ASSERT(!new_ptr || (((UWord) new_ptr) & (alignment - 1)) == 0);
    return new_ptr;
}

static Preload* preloaded = NULL;
static unsigned* res_name = NULL;
static int num_preloaded = 0;

/* Return a pointer to a vector of names of preloaded modules */

Preload* sys_preloaded(void)
{
    HRSRC hRes;
    unsigned char* data;

#define GETWORD(p) (0[p] | 1[p] << 8)
#define GETDWORD(p) (GETWORD(p) | GETWORD(p+2) << 16)


    if (preloaded == NULL) {
	int i;
    ASSERT(beam_module != NULL);
    hRes = FindResource(beam_module, 0, "ERLANG_DICT");
    /* We might have a resource compiler laying out the 0 resource with
       "0" as a textual name instead... */
    if (hRes == NULL) {
	hRes = FindResource(beam_module, "0", "ERLANG_DICT");
    }
    if (hRes == NULL) {
	DWORD n = GetLastError();
	fprintf(stderr, "No ERLANG_DICT resource\n");
	exit(1);
    }
    data = (unsigned char *) LoadResource(beam_module, hRes);
    
    num_preloaded = GETWORD(data);
    if (num_preloaded == 0) {
	fprintf(stderr, "No preloaded modules\n");
	exit(1);
    }

    data += 2;
    preloaded = erts_alloc(ERTS_ALC_T_PRELOADED,
			   (num_preloaded+1)*sizeof(Preload));
    res_name = erts_alloc(ERTS_ALC_T_PRELOADED,
			  (num_preloaded+1)*sizeof(unsigned));
    erts_atomic_add_nob(&sys_misc_mem_sz,
			    (num_preloaded+1)*sizeof(Preload)
			    + (num_preloaded+1)*sizeof(unsigned));
    for (i = 0; i < num_preloaded; i++) {
	int n;
	
	preloaded[i].size = GETDWORD(data);
	data += 4;
	res_name[i] = GETWORD(data);
	data += 2;
	n = GETWORD(data);
	data += 2;
	preloaded[i].name = erts_alloc(ERTS_ALC_T_PRELOADED, n+1);
	erts_atomic_add_nob(&sys_misc_mem_sz, n+1);
	sys_memcpy(preloaded[i].name, data, n);
	preloaded[i].name[n] = '\0';
	data += n;
	DEBUGF(("name: %s; size: %d; resource: %p\n",
		preloaded[i].name, preloaded[i].size, res_name[i]));
    }
    preloaded[i].name = NULL;
    }
    
#undef GETWORD
#undef GETDWORD
    return preloaded;
}

/* Return a pointer to preloaded code for module "module" */
unsigned char* sys_preload_begin(Preload* pp)
{
    HRSRC hRes;
    unsigned resource;
    
    ASSERT(beam_module != NULL);

    resource = res_name[pp-preloaded];
    DEBUGF(("Loading name: %s; size: %d; resource: %p\n",
	    pp->name, pp->size, resource));
    hRes = FindResource(beam_module, (char *) resource, "ERLANG_CODE");
    return pp->code = LoadResource(beam_module, hRes);
}

/* Clean up if allocated */
void sys_preload_end(Preload* pp)
{
}

/* Read a key from console */

int
sys_get_key(int fd)
{
    ASSERT(fd == 0);

    if (win_console) {
        return ConGetKey();
    }

    /*
     * Black magic follows. (Code stolen from get_overlapped_result())
     */

    if (fd_driver_input != NULL && fd_driver_input->thread != (HANDLE)-1) {
	DWORD error;
	int key;

	error = WaitForSingleObject(fd_driver_input->ov.hEvent, INFINITE);
	if (error == WAIT_OBJECT_0) {
	    if (fd_driver_input->bytesTransferred > 0) {
		int n;
		int i;
		char* buf = OV_BUFFER_PTR(fd_driver_input);

		fd_driver_input->bytesTransferred--;
		n = fd_driver_input->bytesTransferred;
		key = buf[0];
		for (i = n; i > 0; i--) {
		    buf[i-1] = buf[i];
		}
		return key;
	    }
	}
    }
    return '*';		/* Error! */
}

/*
 * Returns a human-readable description of the last error.
 * The returned pointer will be valid only as long as last-error()
 * isn't called again.
 */

char* win32_errorstr(int error)
{
  LPTSTR lpBufPtr = erts_tsd_get(win32_errstr_key);
  if (lpBufPtr) {
      LocalFree(lpBufPtr);
  }
  FormatMessage(
		FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM |
		FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL,
		error,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		(LPTSTR) &lpBufPtr,
		0,
		NULL);
  SetLastError(error);
  erts_tsd_set(win32_errstr_key,lpBufPtr);
  return lpBufPtr;
}

char* last_error(void)
{
  return win32_errorstr(GetLastError());
}

static void* sys_func_memzero(void* s, size_t n)
{
    return sys_memzero(s, n);
}

#ifdef DEBUG
static HANDLE hDebugWrite = INVALID_HANDLE_VALUE;

void erl_debug(char *fmt,...)
{
  char sbuf[1024];		/* Temporary buffer. */
  DWORD written;		/* Actual number of chars written. */
  va_list va;

  if (hDebugWrite != INVALID_HANDLE_VALUE) {
    va_start(va, fmt);
    vsprintf(sbuf, fmt, va);
    WriteFile(hDebugWrite, sbuf, strlen(sbuf), &written, NULL);
    va_end(va);
  }
}

static void debug_console(void)
{
  HANDLE hRead;			/* Handle to read end of pipe. */
  SECURITY_ATTRIBUTES sa;
  PROCESS_INFORMATION procInfo;
  STARTUPINFO startInfo;
  BOOL ok;

  /*
   * Create a pipe for communicating with the sub process.
   */

  sa.nLength = sizeof(sa);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;
  if (!CreatePipe(&hRead, &hDebugWrite, &sa, 0)) {
    fprintf(stderr, "Failed to create pipe: %d\n", 
	    GetLastError());
    exit(1);
  }

  startInfo.cb = sizeof(STARTUPINFO);
  startInfo.lpTitle = "Erlang Debug Log";
  startInfo.lpReserved = NULL; 
  startInfo.lpReserved2 = NULL; 
  startInfo.cbReserved2 = 0; 
  startInfo.lpDesktop = NULL;  
  startInfo.dwFlags = STARTF_USESTDHANDLES;
  startInfo.hStdInput = hRead;

  /* The following handles are not intended to be used. */
  startInfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
  startInfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);

  ok = CreateProcess(NULL,
		     "erl_log.exe", /* Application */
		     NULL,	/* Process security attributes. */
		     NULL,	/* Thread security attributes. */
		     TRUE,	/* Handle inheritance flag. */
		     CREATE_NEW_CONSOLE, /* Flags. */
		     NULL,	/* Environment. */
		     NULL,	/* Current directory. */
		     &startInfo,/* Startup info. */
		     &procInfo	/* Process information. */
		     );

  CloseHandle(hRead); 

  if (ok) {
    /*
     * Since we don't use these, close them at once to avoid a resource
     * leak.
     */
    CloseHandle(procInfo.hProcess);
    CloseHandle(procInfo.hThread);
  } else {
    fprintf(stderr, "Create process failed: %s\n", last_error());
    exit(1);
  }
}

void
erl_bin_write(buf, sz, max)
     unsigned char* buf;
     int sz;
     int max;
{
  int i, imax;
  char comma[5] = ",";

  if (hDebugWrite == INVALID_HANDLE_VALUE)
    return;

  if (!sz)
    return;
  if (sz > max)
    imax = max;
  else
    imax = sz;
  
  for (i=0; i<imax; i++) {
    if (i == imax-1) {
      if (sz > max)
	strcpy(comma, ",...");
      else
	comma[0] = 0;
    }
    if (isdigit(buf[i]))
      erl_debug("%u%s", (int)(buf[i]), comma);
    else {
      if (isalpha(buf[i])) {
	erl_debug("%c%s", buf[i], comma);
      }
      else
	erl_debug("%u%s", (int)(buf[i]), comma);
    }
  }
}

#endif /* DEBUG */

void
erl_assert_error(const char* expr, const char* func, const char* file, int line)
{   
    char message[1024];

    erts_snprintf(message, sizeof(message),
	    "File %hs, line %d: %hs", file, line, expr);
    MessageBox(GetActiveWindow(), message, "Assertion failed",
	       MB_OK | MB_ICONERROR);
#if 0
    erl_crash_dump(file, line, "Assertion failed: %hs\n", expr);
#endif
    DebugBreak();
}

	    
static void
check_supported_os_version(void)
{
#if defined(_WIN32_WINNT)
    {
	DWORD major = (_WIN32_WINNT >> 8) & 0xff;
	DWORD minor = _WIN32_WINNT & 0xff;

	if (int_os_version.dwPlatformId != VER_PLATFORM_WIN32_NT
	    || int_os_version.dwMajorVersion < major
	    || (int_os_version.dwMajorVersion == major
		&& int_os_version.dwMinorVersion < minor))
	    erts_exit(1,
		     "Windows version not supported "
		     "(min required: winnt %d.%d)\n",
		     major, minor);
    }
#else
    erts_exit(1,
	     "Windows version not supported "
	     "(min required: win %d.%d)\n",
	     nt_major, nt_minor);
#endif
}


typedef struct {
    int sched_bind_data;
} erts_thr_create_data_t;

/*
 * thr_create_prepare() is called in parent thread before thread creation.
 * Returned value is passed as argument to thr_create_cleanup().
 */
static void *
thr_create_prepare(void)
{
    erts_thr_create_data_t *tcdp;

    tcdp = erts_alloc(ERTS_ALC_T_TMP, sizeof(erts_thr_create_data_t));
    tcdp->sched_bind_data = erts_sched_bind_atthrcreate_prepare();

    return (void *) tcdp;
}


/* thr_create_cleanup() is called in parent thread after thread creation. */
static void
thr_create_cleanup(void *vtcdp)
{
    erts_thr_create_data_t *tcdp = (erts_thr_create_data_t *) vtcdp;

    erts_sched_bind_atthrcreate_parent(tcdp->sched_bind_data);

    erts_free(ERTS_ALC_T_TMP, tcdp);
}

static void
thr_create_prepare_child(void *vtcdp)
{
    erts_thr_create_data_t *tcdp = (erts_thr_create_data_t *) vtcdp;

#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_thread_setup();
#endif /* ERTS_ENABLE_LOCK_COUNT */

    erts_sched_bind_atthrcreate_child(tcdp->sched_bind_data);
}


void
erts_sys_pre_init(void)
{
    erts_thr_init_data_t eid = ERTS_THR_INIT_DATA_DEF_INITER;
    int_os_version.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    GetVersionEx(&int_os_version);
    check_supported_os_version();

    eid.thread_create_child_func = thr_create_prepare_child;
    /* Before creation in parent */
    eid.thread_create_prepare_func = thr_create_prepare;
    /* After creation in parent */
    eid.thread_create_parent_func = thr_create_cleanup;

#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_pre_thr_init();
#endif

    erts_thr_init(&eid);

#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_post_thr_init();
#endif

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_init();
#endif


    erts_init_sys_time_sup();

    erts_atomic_init_nob(&sys_misc_mem_sz, 0);
}

void noinherit_std_handle(DWORD type)
{
    HANDLE h = GetStdHandle(type);
    if (h != 0 && h != INVALID_HANDLE_VALUE) {
	SetHandleInformation(h,HANDLE_FLAG_INHERIT,0);
    }
}
	    

void erl_sys_init(void)
{
    HANDLE handle;

    noinherit_std_handle(STD_OUTPUT_HANDLE);
    noinherit_std_handle(STD_INPUT_HANDLE);
    noinherit_std_handle(STD_ERROR_HANDLE);

    erts_tsd_key_create(&win32_errstr_key,"win32_errstr_key");
    InitializeCriticalSection(&htbc_lock);
    erts_atomic_init_nob(&pipe_creation_counter,0);
    /*
     * Test if we have named pipes or not.
     */

    switch (int_os_version.dwPlatformId) {
    case VER_PLATFORM_WIN32_WINDOWS:
	DEBUGF(("Running on Windows 95"));
	use_named_pipes = FALSE;
	break;
    case VER_PLATFORM_WIN32_NT:
	DEBUGF(("Running on Windows NT"));
#ifdef DISABLE_NAMED_PIPES
	use_named_pipes = FALSE;
#else
	use_named_pipes = TRUE;
#endif
	break;
    default:			/* Unsupported platform. */
	exit(1);
    }
    DEBUGF((" %d.%d, build %d, %s\n",
	    int_os_version.dwMajorVersion, int_os_version.dwMinorVersion,
	    int_os_version.dwBuildNumber, int_os_version.szCSDVersion));

    ASSERT(beam_module != NULL);
    init_console();

    /*
     * The following makes sure that the current directory for the current drive
     * is remembered (in the environment).
     */  

    chdir(".");

    /*
     * Make sure that the standard error handle is valid.
     */
    handle = GetStdHandle(STD_ERROR_HANDLE);
    if (handle == INVALID_HANDLE_VALUE || handle == 0) {
	SetStdHandle(STD_ERROR_HANDLE, GetStdHandle(STD_OUTPUT_HANDLE));
    }
    erts_sys_init_float();

    /* Suppress windows error message popups */
    SetErrorMode(SetErrorMode(0) |
		 SEM_FAILCRITICALERRORS | SEM_NOOPENFILEERRORBOX); 
}
void erts_poll_late_init(void);

void
erl_sys_late_init(void)
{
    /* do nothing */
    erts_poll_late_init();
}
