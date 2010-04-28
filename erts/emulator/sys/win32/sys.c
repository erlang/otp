/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2010. All Rights Reserved.
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



void erts_sys_init_float(void);

void erl_start(int, char**);
void erl_exit(int n, char*, _DOTS_);
void erl_error(char*, va_list);
void erl_crash_dump(char*, int, char*, ...);

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

static void init_console();
static int get_and_remove_option(int* argc, char** argv, const char* option);
static char *get_and_remove_option2(int *argc, char **argv, 
				    const char *option);
static int init_async_io(struct async_io* aio, int use_threads);
static void release_async_io(struct async_io* aio, ErlDrvPort);
static void async_read_file(struct async_io* aio, LPVOID buf, DWORD numToRead);
static int async_write_file(struct async_io* aio, LPVOID buf, DWORD numToWrite);
static int get_overlapped_result(struct async_io* aio,
				 LPDWORD pBytesRead, BOOL wait);
static FUNCTION(BOOL, CreateChildProcess, (char *, HANDLE, HANDLE,
					   HANDLE, LPHANDLE, BOOL,
					   LPVOID, LPTSTR, unsigned, 
					   char **, int *));
static int create_pipe(LPHANDLE, LPHANDLE, BOOL, BOOL);
static int ApplicationType(const char* originalName, char fullPath[MAX_PATH],
			   BOOL search_in_path, BOOL handle_quotes,
			   int *error_return);

HANDLE erts_service_event;

#ifdef ERTS_SMP
static erts_smp_tsd_key_t win32_errstr_key;
#endif

static erts_smp_atomic_t pipe_creation_counter;

static erts_smp_mtx_t sys_driver_data_lock;


/* Results from ApplicationType is one of */
#define APPL_NONE 0
#define APPL_DOS  1
#define APPL_WIN3X 2
#define APPL_WIN32 3

static FUNCTION(int, driver_write, (long, HANDLE, byte*, int));
static void common_stop(int);
static int create_file_thread(struct async_io* aio, int mode);
#ifdef ERTS_SMP
static void close_active_handles(ErlDrvPort, const HANDLE* handles, int cnt);
static DWORD WINAPI threaded_handle_closer(LPVOID param);
#endif
static DWORD WINAPI threaded_reader(LPVOID param);
static DWORD WINAPI threaded_writer(LPVOID param);
static DWORD WINAPI threaded_exiter(LPVOID param);

#ifdef DEBUG
static void debug_console(void);
#endif

BOOL WINAPI ctrl_handler(DWORD dwCtrlType);

#define PORT_BUFSIZ 4096

#define PORT_FREE (-1)
#define PORT_EXITING (-2)

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

#ifdef ERTS_SMP
static BOOL (WINAPI *fpCancelIoEx)(HANDLE,LPOVERLAPPED);
#endif

/* This is the system's main function (which may or may not be called "main")
   - do general system-dependent initialization
   - call erl_start() to parse arguments and do other init
*/

static erts_smp_atomic_t sys_misc_mem_sz;

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

Uint
erts_sys_misc_mem_sz(void)
{
    Uint res = (Uint) erts_check_io_size();
    res += (Uint) erts_smp_atomic_read(&sys_misc_mem_sz);
    return res;
}

/*
 * Reset the terminal to the original settings on exit
 */
void sys_tty_reset(int exit_code)
{
    if (exit_code > 0)
	ConWaitForExit();
    else
	ConNormalExit();
}

void erl_sys_args(int* argc, char** argv)
{
    char *event_name;
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

void
erts_sys_prepare_crash_dump(void)
{
    /* Windows - free file descriptors are hopefully available */
    return;
}

static void
init_console()
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

int sys_max_files() 
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
get_and_remove_option(argc, argv, option)
    int* argc;			/* Number of arguments. */
    char* argv[];		/* The argument vector. */
    const char* option;		/* Option to search for and remove. */
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
os_flavor(namebuf, size)
char* namebuf;			/* Where to return the name. */
unsigned size;			/* Size of name buffer. */
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

#define DF_OVR_READY	1	/* Overlapped result is ready. */
#define DF_EXIT_THREAD	2	/* The thread should exit. */
#define DF_XLAT_CR	4	/* The thread should translate CRs. */
#define DF_DROP_IF_INVH 8       /* Drop packages instead of crash if
				   invalid handle (stderr) */

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
#ifdef ERTS_SMP
  int async_io_active;          /* if true, a close of the file will signal the event in ov */
#endif
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
  DWORD pendingError;		/* Used to delay presentating an error to Erlang
				 * until the check_io function is entered.
				 */
  DWORD bytesTransferred;	/* Bytes read or write in the last operation.
				 * Valid only when DF_OVR_READY is set.
				 */
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

typedef struct driver_data {
    int totalNeeded;		/* Total number of bytes needed to fill
				 * up the packet header or packet. */
    int bytesInBuffer;		/* Number of bytes read so far in
				 * the input buffer.
				 */
    int inBufSize;		/* Size of input buffer. */
    byte *inbuf;		/* Buffer to use for overlapped read. */
    int outBufSize;		/* Size of output buffer. */
    byte *outbuf;		/* Buffer to use for overlapped write. */
    ErlDrvPort port_num;		/* The port number. */
    int packet_bytes;		/* 0: continous stream, 1, 2, or 4: the number
				 * of bytes in the packet header.
				 */
    HANDLE port_pid;		/* PID of the port process. */
    AsyncIo in;			/* Control block for overlapped reading. */
    AsyncIo out;		/* Control block for overlapped writing. */
    int report_exit;            /* Do report exit status for the port */
} DriverData;

static DriverData* driver_data;	/* Pointer to array of driver data. */

/* Driver interfaces */
static ErlDrvData spawn_start(ErlDrvPort, char*, SysDriverOpts*);
static ErlDrvData fd_start(ErlDrvPort, char*, SysDriverOpts*);
static ErlDrvData vanilla_start(ErlDrvPort, char*, SysDriverOpts*);
static int spawn_init(void);
static int fd_init(void);
static void fd_stop(ErlDrvData);
static void stop(ErlDrvData);
static void output(ErlDrvData, char*, int);
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

#if defined(USE_THREADS) && !defined(ERTS_SMP)

static int  async_drv_init(void);
static ErlDrvData async_drv_start(ErlDrvPort, char*, SysDriverOpts*);
static void async_drv_stop(ErlDrvData);
static void async_drv_input(ErlDrvData, ErlDrvEvent);

/* INTERNAL use only */

void null_output(ErlDrvData drv_data, char* buf, int len)
{
}

void null_ready_output(ErlDrvData drv_data, ErlDrvEvent event)
{
}

struct erl_drv_entry async_driver_entry = {
    async_drv_init,
    async_drv_start,
    async_drv_stop,
    null_output,
    async_drv_input,
    null_ready_output,
    "async",
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

#endif

/*
 * Initialises a DriverData structure.
 *
 * Results: Returns a pointer to a DriverData structure, or NULL
 * if the initialsation failed.
 */

static DriverData*
new_driver_data(port_num, packet_bytes, wait_objs_required, use_threads)
    int port_num;		/* The port number. */
    int packet_bytes;		/* Number of bytes in header. */
    int wait_objs_required;	/* The number objects this port is going
				/* wait for (typically 1 or 2). */
    int use_threads;		/* TRUE if threads are intended to be used. */
{
    DriverData* dp;
    
    erts_smp_mtx_lock(&sys_driver_data_lock);

    DEBUGF(("new_driver_data(port_num %d, pb %d)\n",
	    port_num, packet_bytes));

    /*
     * We used to test first at all that there is enough room in the
     * array used by WaitForMultipleObjects(), but that is not necessary
     * any more, since driver_select() can't fail.
     */

    /*
     * Search for a free slot.
     */

    for (dp = driver_data; dp < driver_data+max_files; dp++) {
	if (dp->port_num == PORT_FREE) {
	    dp->bytesInBuffer = 0;
	    dp->totalNeeded = packet_bytes;
	    dp->inBufSize = PORT_BUFSIZ;
	    dp->inbuf = DRV_BUF_ALLOC(dp->inBufSize);
	    if (dp->inbuf == NULL) {
		erts_smp_mtx_unlock(&sys_driver_data_lock);
		return NULL;
	    }
	    erts_smp_atomic_add(&sys_misc_mem_sz, dp->inBufSize);
	    dp->outBufSize = 0;
	    dp->outbuf = NULL;
	    dp->port_num = port_num;
	    dp->packet_bytes = packet_bytes;
	    dp->port_pid = INVALID_HANDLE_VALUE;
	    if (init_async_io(&dp->in, use_threads) == -1)
		break;
	    if (init_async_io(&dp->out, use_threads) == -1)
		break;
	    erts_smp_mtx_unlock(&sys_driver_data_lock);
	    return dp;
	}
    }

    /*
     * Error or no free driver data.
     */

    if (dp < driver_data+max_files) {
	release_async_io(&dp->in, dp->port_num);
	release_async_io(&dp->out, dp->port_num);
    }
    erts_smp_mtx_unlock(&sys_driver_data_lock);
    return NULL;
}

static void
release_driver_data(DriverData* dp)
{
    erts_smp_mtx_lock(&sys_driver_data_lock);

#ifdef ERTS_SMP
    if (fpCancelIoEx != NULL) {
	if (dp->in.thread == (HANDLE) -1 && dp->in.fd != INVALID_HANDLE_VALUE) {
	    (*fpCancelIoEx)(dp->in.fd, NULL);
	}
	if (dp->out.thread == (HANDLE) -1 && dp->out.fd != INVALID_HANDLE_VALUE) {
	    (*fpCancelIoEx)(dp->out.fd, NULL);
	}
    }
    else {
	/* This is a workaround for the fact that CancelIo cant cancel
	   requests issued by another thread and that we cant use
	   CancelIoEx as that's only availabele in Vista etc.
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
		handles[i++] = dp->in.ov.hEvent;
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
		handles[i++] = dp->out.ov.hEvent;
		dp->out.ov.hEvent = NULL;
	    }
	    DEBUGF(("...done\n"));
	}
	if (i > 0) {
	    close_active_handles(dp->port_num, handles, i);
	}
    }
#else
	if (dp->in.thread == (HANDLE) -1 && dp->in.fd != INVALID_HANDLE_VALUE) {
	     CancelIo(dp->in.fd); 
	}
	if (dp->out.thread == (HANDLE) -1 && dp->out.fd != INVALID_HANDLE_VALUE) {
	    CancelIo(dp->out.fd); 
	}
#endif

    if (dp->inbuf != NULL) {
	ASSERT(erts_smp_atomic_read(&sys_misc_mem_sz) >= dp->inBufSize);
	erts_smp_atomic_add(&sys_misc_mem_sz, -1*dp->inBufSize);
	DRV_BUF_FREE(dp->inbuf);
	dp->inBufSize = 0;
	dp->inbuf = NULL;
    }
    ASSERT(dp->inBufSize == 0);

    if (dp->outbuf != NULL) {
	ASSERT(erts_smp_atomic_read(&sys_misc_mem_sz) >= dp->outBufSize);
	erts_smp_atomic_add(&sys_misc_mem_sz, -1*dp->outBufSize);
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

    dp->port_num = PORT_FREE;
    erts_smp_mtx_unlock(&sys_driver_data_lock);
}

#ifdef ERTS_SMP

struct handles_to_be_closed
{
    int cnt;
    HANDLE handles[2];
};

static void close_active_handles(ErlDrvPort port_num, const HANDLE* handles, int cnt)
{
    DWORD tid;
    HANDLE thread;
    int i;
    struct handles_to_be_closed* htbc = erts_alloc(ERTS_ALC_T_DRV_TAB,
						   sizeof(struct handles_to_be_closed));
    htbc->cnt = cnt;
    for (i=0; i < cnt; ++i) {
	htbc->handles[i] = handles[i];
	(void) driver_select(port_num, (ErlDrvEvent)handles[i],
			     ERL_DRV_USE_NO_CALLBACK, 0);
    }
    thread = (HANDLE *) _beginthreadex(NULL, 0, threaded_handle_closer, htbc, 0, &tid);
    CloseHandle(thread);
}


static DWORD WINAPI
threaded_handle_closer(LPVOID param)
{
    struct handles_to_be_closed* htbc = (struct handles_to_be_closed*) param;
    int i;
    DEBUGF(("threaded_handle_closer waiting for %d handles\r\n",htbc->cnt));
    WaitForMultipleObjects(htbc->cnt, htbc->handles, TRUE, INFINITE);
    for (i=0; i < htbc->cnt; ++i) {
	CloseHandle(htbc->handles[i]);
    }
    erts_free(ERTS_ALC_T_DRV_TAB, htbc);
    DEBUGF(("threaded_handle_closer terminating\r\n"));
    return 0;
}
#endif /* ERTS_SMP */

/*
 * Stores input and output file descriptors in the DriverData structure,
 * and calls driver_select().
 *
 * This function fortunately can't fail!
 */

static ErlDrvData
set_driver_data(dp, ifd, ofd, read_write, report_exit)
    DriverData* dp;
    HANDLE ifd;
    HANDLE ofd;
    int read_write;
    int report_exit;
{
    int index = dp - driver_data;
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
    return (ErlDrvData)index;
}

/*
 * Initialises an AsyncIo structure.
 */

static int
init_async_io(AsyncIo* aio, int use_threads)
{
    aio->flags = 0;
    aio->thread = (HANDLE) -1;
    aio->fd = INVALID_HANDLE_VALUE;
    aio->ov.hEvent = NULL;
    aio->ov.Offset = 0L;
    aio->ov.OffsetHigh = 0L;
    aio->ioAllowed = NULL;
    aio->pendingError = 0;
    aio->bytesTransferred = 0;
#ifdef ERTS_SMP
    aio->async_io_active = 0;
#endif
    aio->ov.hEvent = CreateManualEvent(FALSE);
    if (aio->ov.hEvent == NULL)
	return -1;
    if (use_threads) {
	aio->ioAllowed = CreateAutoEvent(FALSE);
	if (aio->ioAllowed == NULL)
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

    if (aio->ov.hEvent != NULL) {
	(void) driver_select(port_num,
			     (ErlDrvEvent)aio->ov.hEvent,
			     ERL_DRV_USE, 0);
	/* was CloseHandle(aio->ov.hEvent); */
    }

    aio->ov.hEvent = NULL;

    if (aio->ioAllowed != NULL)
	CloseHandle(aio->ioAllowed);
    aio->ioAllowed = NULL;
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
async_read_file(aio, buf, numToRead)
    AsyncIo* aio;		/* Pointer to driver data. */
    LPVOID buf;			/* Pointer to buffer to receive data. */
    DWORD numToRead;		/* Number of bytes to read. */
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
#ifdef ERTS_SMP
	aio->async_io_active = 1; /* Will get 0 when the event actually happened */
#endif
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
async_write_file(aio, buf, numToWrite)
    AsyncIo* aio;		/* Pointer to async control block. */
    LPVOID buf;			/* Pointer to buffer with data to write. */
    DWORD numToWrite;		/* Number of bytes to write. */
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
#ifdef ERTS_SMP
	aio->async_io_active = 1; /* Will get 0 when the event actually happened */
#endif
	if (WriteFile(aio->fd, buf, numToWrite,
		      &aio->bytesTransferred, &aio->ov)) {
	    DEBUGF(("async_write_file: WriteFile() suceeded: %d bytes\n",
		    aio->bytesTransferred));
#ifdef ERTS_SMP
	    aio->async_io_active = 0; /* The event will not be signalled */
#endif
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
get_overlapped_result(aio, pBytesRead, wait)
    AsyncIo* aio;		/* Pointer to async control block. */
    LPDWORD pBytesRead;		/* Where to place the number of bytes
				 * transferred.
				 */
    BOOL wait;			/* If true, wait until result is ready. */
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
spawn_init()
{
    int i;
#ifdef ERTS_SMP
    HMODULE module = GetModuleHandle("kernel32");
    fpCancelIoEx = (module != NULL) ?
	(BOOL (WINAPI *)(HANDLE,LPOVERLAPPED))
      GetProcAddress(module,"CancelIoEx") : NULL;
    DEBUGF(("fpCancelIoEx = %p\r\n", fpCancelIoEx));
#endif
    driver_data = (struct driver_data *)
	erts_alloc(ERTS_ALC_T_DRV_TAB, max_files * sizeof(struct driver_data));
    erts_smp_atomic_add(&sys_misc_mem_sz, max_files*sizeof(struct driver_data));
    for (i = 0; i < max_files; i++)
	driver_data[i].port_num = PORT_FREE;

    return 0;
}

static ErlDrvData
spawn_start(ErlDrvPort port_num, char* name, SysDriverOpts* opts)
{
    HANDLE hToChild = INVALID_HANDLE_VALUE; /* Write handle to child. */
    HANDLE hFromChild = INVALID_HANDLE_VALUE; /* Read handle from child. */
    HANDLE hChildStdin = INVALID_HANDLE_VALUE;		/* Child's stdin. */
    HANDLE hChildStdout = INVALID_HANDLE_VALUE;	/* Child's stout. */
    HANDLE hChildStderr = INVALID_HANDLE_VALUE;	/* Child's sterr. */
    int close_child_stderr = 0;
    DriverData* dp;		/* Pointer to driver data. */
    ErlDrvData retval = ERL_DRV_ERROR_GENERAL; /* Return value. */
    int ok;
    int neededSelects = 0;
    SECURITY_ATTRIBUTES sa = {sizeof(SECURITY_ATTRIBUTES), NULL, TRUE};
    char* envir = opts->envir;
    int errno_return = -1;
    
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

    DEBUGF(("Spawning \"%s\"\n", name));
    envir = win_build_environment(envir);
    ok = CreateChildProcess(name, 
			    hChildStdin, 
			    hChildStdout,
			    hChildStderr,
			    &dp->port_pid,
			    opts->hide_window,
			    (LPVOID) envir,
			    (LPTSTR) opts->wd,
			    opts->spawn_type,
			    opts->argv, 
			    &errno_return);
    CloseHandle(hChildStdin);
    CloseHandle(hChildStdout);
    if (close_child_stderr && hChildStderr != INVALID_HANDLE_VALUE &&
	hChildStderr != 0) {
	CloseHandle(hChildStderr);
    }
    if (envir != NULL) {
	erts_free(ERTS_ALC_T_ENVIRONMENT, envir);
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

static int
create_file_thread(AsyncIo* aio, int mode)
{
    DWORD tid;			/* Id for thread. */

    aio->thread = (HANDLE)
	_beginthreadex(NULL, 0, 
		       (mode & DO_WRITE) ? threaded_writer : threaded_reader,
		       aio, 0, &tid);

    return aio->thread != (HANDLE) -1;
}

/* 
 *  A helper function used by CreateChildProcess().
 *  Parses a command line with arguments and returns the length of the
 *  first part containing the program name.
 *  Example: input = "\"Program Files\"\\erl arg1 arg2"
 *  gives 19 as result.
 *  The length returned is equivalent with length(argv[0]) if the
 *  comman line should have been prepared by _setargv for the main function
*/
int parse_command(char* cmd){
#define NORMAL 2
#define STRING 1
#define STOP 0
    int i =0;
    int state = NORMAL;
    while (cmd[i]) {
	switch (cmd[i]) {
	case '"':
	    if (state == NORMAL) 
		state = STRING;
	    else
		state = NORMAL;
	    break;
	case '\\':
	    if ((state == STRING) && (cmd[i+1]=='"'))
		i++;
	    break;
	case ' ':
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

BOOL need_quotes(char *str)
{
    int in_quote = 0;
    int backslashed = 0;
    int naked_space = 0;
    while (*str != '\0') {
	switch (*str) {
	case '\\' :
	    backslashed = !backslashed;
	    break;
	case '"':
	    if (backslashed) {
		backslashed=0;
	    } else {
		in_quote = !in_quote;
	    }
	    break;
	case ' ':
	    backslashed = 0;
	    if (!(backslashed || in_quote)) {
		naked_space++;
	    }
	    break;
	default:
	    backslashed = 0;
	}
	++str;
    }
    return (naked_space > 0);
}
	    
	    

/*
 *----------------------------------------------------------------------
 *
 * CreateChildProcess --
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
CreateChildProcess
(
 char *origcmd,  /* Command line for child process (including
		  * name of executable). Or whole executable if st is
		  * ERTS_SPAWN_EXECUTABLE
		  */
 HANDLE hStdin,  /* The standard input handle for child. */
 HANDLE hStdout, /* The standard output handle for child. */ 
 HANDLE hStderr, /* The standard error handle for child. */
 LPHANDLE phPid, /* Pointer to variable to received PID. */
 BOOL hide,      /* Hide the window unconditionally. */
 LPVOID env,     /* Environment for the child */
 LPTSTR wd,      /* Working dir for the child */
 unsigned st,    /* Flags for spawn, tells us how to interpret origcmd */
 char **argv,     /* Argument vector if given. */
 int *errno_return /* Place to put an errno in in case of failure */
 )
{ 
    PROCESS_INFORMATION piProcInfo = {0};
    STARTUPINFO siStartInfo = {0};
    BOOL ok = FALSE;
    int applType;
    /* Not to be changed for different types of executables */
    int staticCreateFlags = GetPriorityClass(GetCurrentProcess()); 
    int createFlags = DETACHED_PROCESS;
    char *newcmdline = NULL;
    char execPath[MAX_PATH];
    int cmdlength;
    char* thecommand;
    LPTSTR appname = NULL;
    HANDLE hProcess = GetCurrentProcess();
    
    *errno_return = -1;

    siStartInfo.cb = sizeof(STARTUPINFO); 
    siStartInfo.dwFlags = STARTF_USESTDHANDLES;
    siStartInfo.hStdInput = hStdin;
    siStartInfo.hStdOutput = hStdout;
    siStartInfo.hStdError = hStderr;


    if (st != ERTS_SPAWN_EXECUTABLE) {
	/*
	 * Parse out the program name from the command line (it can be quoted and
	 * contain spaces).
	 */
	newcmdline = erts_alloc(ERTS_ALC_T_TMP, 2048);
	cmdlength = parse_command(origcmd);
	thecommand = (char *) erts_alloc(ERTS_ALC_T_TMP, cmdlength+1);
	strncpy(thecommand, origcmd, cmdlength);
	thecommand[cmdlength] = '\0';
	DEBUGF(("spawn command: %s\n", thecommand));
    
	applType = ApplicationType(thecommand, execPath, TRUE, 
				   TRUE, errno_return);
	DEBUGF(("ApplicationType returned for (%s) is %d\n", thecommand, applType));
	erts_free(ERTS_ALC_T_TMP, (void *) thecommand);
	if (applType == APPL_NONE) {
	    erts_free(ERTS_ALC_T_TMP,newcmdline);
	    return FALSE;
	}
	newcmdline[0] = '\0'; 

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
	    strcat(newcmdline, "cmd.exe /c ");
	} else if (hide) {
	    DEBUGF(("hiding window\n"));
	    siStartInfo.wShowWindow = SW_HIDE;
	    siStartInfo.dwFlags |= STARTF_USESHOWWINDOW;
	    createFlags = 0;
	}

	strcat(newcmdline, execPath);
	strcat(newcmdline, origcmd+cmdlength);
    } else { /* ERTS_SPAWN_EXECUTABLE */
	int run_cmd = 0;
	applType = ApplicationType(origcmd, execPath, FALSE, FALSE, 
				   errno_return);
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
	    char cmdPath[MAX_PATH];
	    int cmdType;
	    cmdType = ApplicationType("cmd.exe", cmdPath, TRUE, FALSE, errno_return);
	    if (cmdType == APPL_NONE || cmdType == APPL_DOS) {
		return FALSE;
	    }
	    appname = (char *) erts_alloc(ERTS_ALC_T_TMP, strlen(cmdPath)+1);
	    strcpy(appname,cmdPath);
	} else {
	    appname = (char *) erts_alloc(ERTS_ALC_T_TMP, strlen(execPath)+1);
	    strcpy(appname,execPath);
	}
	if (argv == NULL) {
	    BOOL orig_need_q = need_quotes(execPath);
	    char *ptr;
	    int ocl = strlen(execPath);
	    if (run_cmd) {
		newcmdline = (char *) erts_alloc(ERTS_ALC_T_TMP, 
						 ocl + ((orig_need_q) ? 3 : 1)
						 + 11);
		memcpy(newcmdline,"cmd.exe /c ",11);
		ptr = newcmdline + 11;
	    } else {
		newcmdline = (char *) erts_alloc(ERTS_ALC_T_TMP, 
						 ocl + ((orig_need_q) ? 3 : 1));
		ptr = newcmdline;
	    }
	    if (orig_need_q) {
		*ptr++ = '"';
	    }
	    memcpy(ptr,execPath,ocl);
	    ptr += ocl;
	    if (orig_need_q) {
		*ptr++ = '"';
	    }
	    *ptr = '\0';
	} else {
	    int sum = 1; /* '\0' */
	    char **ar = argv;
	    char *n;
	    char *save_arg0 = NULL;
	    if (argv[0] == erts_default_arg0 || run_cmd) {
		save_arg0 = argv[0];
		argv[0] = execPath;
	    }
	    if (run_cmd) {
		sum += 11; /* cmd.exe /c */
	    }
	    while (*ar != NULL) {
		sum += strlen(*ar);
		if (need_quotes(*ar)) {
		    sum += 2; /* quotes */
		}
		sum++; /* space */
		++ar;
	    }
	    ar = argv;
	    newcmdline = erts_alloc(ERTS_ALC_T_TMP, sum);
	    n = newcmdline;
	    if (run_cmd) {
		memcpy(n,"cmd.exe /c ",11);
		n += 11;
	    }
	    while (*ar != NULL) {
		int q = need_quotes(*ar);
		sum = strlen(*ar);
		if (q) {
		    *n++ = '"';
		}
		memcpy(n,*ar,sum);
		n += sum;
		if (q) {
		    *n++ = '"';
		}
		*n++ = ' ';
		++ar;
	    }
	    ASSERT(n > newcmdline);
	    *(n-1) = '\0';
	    if (save_arg0 != NULL) {
		argv[0] = save_arg0;
	    }
	}	    
	    
    }
    DEBUGF(("Creating child process: %s, createFlags = %d\n", newcmdline, createFlags));
    ok = CreateProcess(appname, 
		       newcmdline, 
		       NULL, 
		       NULL, 
		       TRUE, 
		       createFlags | staticCreateFlags, 
		       env, 
		       wd, 
		       &siStartInfo, 
		       &piProcInfo);

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
    
    if (applType == APPL_DOS) {
	WaitForSingleObject(hProcess, 50);
    }
    
    /* 
     * When an application spawns a process repeatedly, a new thread 
     * instance will be created for each process but the previous 
     * instances may not be cleaned up.  This results in a significant 
     * virtual memory loss each time the process is spawned.  If there 
     * is a WaitForInputIdle() call between CreateProcess() and
     * CloseHandle(), the problem does not occur. PSS ID Number: Q124121
     */
    
    WaitForInputIdle(piProcInfo.hProcess, 5000);
    
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
    char pipe_name[128];	/* Name of pipe. */
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

    calls = (Uint) erts_smp_atomic_inctest(&pipe_creation_counter);
    sprintf(pipe_name, "\\\\.\\pipe\\erlang44_%d_%d",
	    getpid(), calls);

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




static int ApplicationType
(
 const char *originalName, /* Name of the application to find. */ 
 char fullPath[MAX_PATH],  /* Filled with complete path to 
			    * application. */
 BOOL search_in_path,      /* If we should search the system wide path */
 BOOL handle_quotes,       /* If we should handle quotes around executable */
 int *error_return         /* A place to put an error code */
 )
{
    int applType, i;
    HANDLE hFile;
    char *ext, *rest;
    char buf[2];
    DWORD read;
    IMAGE_DOS_HEADER header;
    static char extensions[][5] = {"", ".com", ".exe", ".bat"};
    int is_quoted;
    int len;

    /* Look for the program as an external program.  First try the name
     * as it is, then try adding .com, .exe, and .bat, in that order, to
     * the name, looking for an executable.
     * NOTE! that we does not support execution of .com programs on Windows NT
     * 
     *
     * Using the raw SearchPath() procedure doesn't do quite what is 
     * necessary.  If the name of the executable already contains a '.' 
     * character, it will not try appending the specified extension when
     * searching (in other words, SearchPath will not find the program 
     * "a.b.exe" if the arguments specified "a.b" and ".exe").   
     * So, first look for the file as it is named.  Then manually append 
     * the extensions, looking for a match.  (')
     */

    len = strlen(originalName);
    is_quoted = handle_quotes && len > 0 && originalName[0] == '"' && 
	originalName[len-1] == '"';

    applType = APPL_NONE;
    *error_return = ENOENT;
    for (i = 0; i < (int) (sizeof(extensions) / sizeof(extensions[0])); i++) {
	if(is_quoted) {
	   lstrcpyn(fullPath, originalName+1, MAX_PATH - 7); 
	   len = strlen(fullPath);
	   if(len > 0) {
	       fullPath[len-1] = '\0';
	   }
	} else {
	    lstrcpyn(fullPath, originalName, MAX_PATH - 5);
	}
	lstrcat(fullPath, extensions[i]);
	SearchPath((search_in_path) ? NULL : ".", fullPath, NULL, MAX_PATH, fullPath, &rest);

	/*
	 * Ignore matches on directories or data files, return if identified
	 * a known type.
	 */

	if (GetFileAttributes(fullPath) & FILE_ATTRIBUTE_DIRECTORY) {
	    continue;
	}

	ext = strrchr(fullPath, '.');
	if ((ext != NULL) && (strcmpi(ext, ".bat") == 0)) {
	    *error_return = EACCES;
	    applType = APPL_DOS;
	    break;
	}

	hFile = CreateFile(fullPath, GENERIC_READ, FILE_SHARE_READ, NULL, 
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
	    if ((ext != NULL) && (strcmpi(ext, ".com") == 0)) {
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

	GetShortPathName(fullPath, fullPath, MAX_PATH);
    }
    if (is_quoted) {
	/* restore quotes on quoted program name */
	len = strlen(fullPath);
	memmove(fullPath+1,fullPath,len);
	fullPath[0]='"';
	fullPath[len+1]='"';
	fullPath[len+2]='\0';
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
	if (!ReadFile(aio->fd, buf, numToRead, &aio->bytesTransferred, NULL))
	    aio->pendingError = GetLastError();
	else if (aio->flags & DF_XLAT_CR) {
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
    DWORD numToWrite;
    int ok;
  
    for (;;) {
	WaitForSingleObject(aio->ioAllowed, INFINITE);
	if (aio->flags & DF_EXIT_THREAD)
	    break;
	buf = OV_BUFFER_PTR(aio);
	numToWrite = OV_NUM_TO_READ(aio);
	aio->pendingError = 0;
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
	SetEvent(aio->ov.hEvent);
	if (aio->pendingError != NO_ERROR || aio->bytesTransferred == 0)
	    break;
	if (aio->flags & DF_EXIT_THREAD)
	    break;
    }
    CloseHandle(aio->fd);
    aio->fd = INVALID_HANDLE_VALUE;
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

static ErlDrvData
fd_start(ErlDrvPort port_num, char* name, SysDriverOpts* opts)
{
    DriverData* dp;
    int is_std_error = (opts->ofd == 2);
    
    opts->ifd = (int) translate_fd(opts->ifd);
    opts->ofd = (int) translate_fd(opts->ofd);
    if ((dp = new_driver_data(port_num, opts->packet_bytes, 2, TRUE)) == NULL)
	return ERL_DRV_ERROR_GENERAL;
    
    if (!create_file_thread(&dp->in, DO_READ)) {
	dp->port_num = PORT_FREE;
	return ERL_DRV_ERROR_GENERAL;
    }

    if (!create_file_thread(&dp->out, DO_WRITE)) {
	dp->port_num = PORT_FREE;
	return ERL_DRV_ERROR_GENERAL;
    }
    
    fd_driver_input = &(dp->in);
    dp->in.flags = DF_XLAT_CR;
    if (is_std_error) {
	dp->out.flags |= DF_DROP_IF_INVH; /* Just drop messages if stderror
					     is an invalid handle */
    }
    return set_driver_data(dp, opts->ifd, opts->ofd, opts->read_write, 0);
}

static void fd_stop(ErlDrvData d)
{
  int fd = (int)d;
  /*
   * I don't know a clean way to terminate the threads
   * (TerminateThread() doesn't release the stack),
   * so will we'll let the threads live.  Normally, the fd
   * driver is only used to support the -oldshell option,
   * so this shouldn't be a problem in practice.
   *
   * Since we will not attempt to terminate the threads,
   * better not close the input or output files either.
   */

  driver_data[fd].in.thread = (HANDLE) -1;
  driver_data[fd].out.thread = (HANDLE) -1;
  driver_data[fd].in.fd = INVALID_HANDLE_VALUE;
  driver_data[fd].out.fd = INVALID_HANDLE_VALUE;

  /*return */ common_stop(fd);
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
stop(ErlDrvData index)
{
    common_stop((int)index);
}

static void common_stop(int index)
{
    DriverData* dp = driver_data+index;

    DEBUGF(("common_stop(%d)\n", index));

    if (dp->in.ov.hEvent != NULL) {
	(void) driver_select(dp->port_num,
			     (ErlDrvEvent)dp->in.ov.hEvent,
			     ERL_DRV_READ, 0);
    }
    if (dp->out.ov.hEvent != NULL) {
	(void) driver_select(dp->port_num,
			     (ErlDrvEvent)dp->out.ov.hEvent,
			     ERL_DRV_WRITE, 0);
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
	dp->port_num = PORT_EXITING;
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
	dp->out.flags = DF_EXIT_THREAD;
	SetEvent(dp->out.ioAllowed);
	handles[i++] = dp->out.thread;
    }
    if (dp->in.thread != (HANDLE) -1) {
	dp->in.flags = DF_EXIT_THREAD;
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
output(ErlDrvData drv_data, char* buf, int len)
/*     long drv_data;		/* The slot to use in the driver data table.
				 * For Windows NT, this is *NOT* a file handle.
				 * The handle is found in the driver data.
				 */
/*     char *buf;			/* Pointer to data to write to the port program. */
/*     int len;			/* Number of bytes to write. */
{
    DriverData* dp;
    int pb;			/* The header size for this port. */
    int port_num;		/* The actual port number (for diagnostics). */
    char* current;

    dp = driver_data + (int)drv_data;
    if ((port_num = dp->port_num) == -1)
	return ; /*-1;*/

    pb = dp->packet_bytes;

    if ((pb+len) == 0)
	return ; /* 0; */

    /*
     * Check that the message can be sent with given header length.
     */

    if ((pb == 2 && len > 65535) || (pb == 1 && len > 255)) {
	driver_failure_posix(port_num, EINVAL);
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
	driver_failure_posix(port_num, ENOMEM);
	return ; /* -1; */
    }

    dp->outBufSize = pb+len;
    erts_smp_atomic_add(&sys_misc_mem_sz, dp->outBufSize);

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
	set_busy_port(port_num, 1);
    } else {
	dp->out.ov.Offset += pb+len; /* For vanilla driver. */
	/* XXX OffsetHigh should be changed too. */
	ASSERT(erts_smp_atomic_read(&sys_misc_mem_sz) >= dp->outBufSize);
	erts_smp_atomic_add(&sys_misc_mem_sz, -1*dp->outBufSize);
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
 *	If the port is working in the continous stream mode (packet_bytes == 0),
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
    DriverData* dp;
    int pb;

    dp = driver_data+(int)drv_data;
    pb = dp->packet_bytes;
#ifdef ERTS_SMP
    if(dp->in.thread == (HANDLE) -1) {
	dp->in.async_io_active = 0;
    }
#endif
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
	if (pb == 0) { /* Continous stream. */
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
			ASSERT(erts_smp_atomic_read(&sys_misc_mem_sz) >= dp->inBufSize);
			erts_smp_atomic_add(&sys_misc_mem_sz,
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
    DriverData* dp = driver_data + (int)drv_data;
    int error;

#ifdef ERTS_SMP
    if(dp->out.thread == (HANDLE) -1) {
	dp->out.async_io_active = 0;
    }
#endif
    DEBUGF(("ready_output(%d, 0x%x)\n", drv_data, ready_event));
    set_busy_port(dp->port_num, 0);
    if (!(dp->outbuf)) {
	/* Happens because event sometimes get signalled during a succesful
	   write... */
	return;
    }
    ASSERT(erts_smp_atomic_read(&sys_misc_mem_sz) >= dp->outBufSize);
    erts_smp_atomic_add(&sys_misc_mem_sz, -1*dp->outBufSize);
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
void sys_get_pid(char *buffer){
    DWORD p = GetCurrentProcessId();
    /* The pid is scalar and is an unsigned long. */
    sprintf(buffer,"%lu",(unsigned long) p);
}

void
sys_init_io(void)
{
    
    /* Now heres an icky one... This is called before drivers are, so we
       can change our view of the number of open files possible.
       We estimate the number to twice the amount of ports. 
       We really dont know on windows, do we? */
    max_files = 2*erts_max_ports;
    
#ifdef USE_THREADS
#ifdef ERTS_SMP
    if (init_async(-1) < 0)
	erl_exit(1, "Failed to initialize async-threads\n");
#else
    {
	/* This is special stuff, starting a driver from the 
	 * system routines, but is a nice way of handling stuff
	 * the erlang way
	 */
	SysDriverOpts dopts;
	int ret;

	sys_memset((void*)&dopts, 0, sizeof(SysDriverOpts));
	add_driver_entry(&async_driver_entry);
	ret = erts_open_driver(NULL, NIL, "async", &dopts, NULL);
	DEBUGF(("open_driver = %d\n", ret));
	if (ret < 0)
	    erl_exit(1, "Failed to open async driver\n");
	erts_port[ret].status |= ERTS_PORT_SFLG_IMMORTAL;
    }
#endif
#endif
}

#ifdef ERTS_SMP
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
#endif

void erts_sys_alloc_init(void)
{
    elib_ensure_initialized();
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
    erts_smp_atomic_add(&sys_misc_mem_sz,
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
	erts_smp_atomic_add(&sys_misc_mem_sz, n+1);
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
#ifdef SMP
  LPTSTR lpBufPtr = erts_smp_tsd_get(win32_errstr_key);
#else
  static LPTSTR lpBufPtr = NULL;
#endif
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
#ifdef ERTS_SMP
  erts_smp_tsd_set(win32_errstr_key,lpBufPtr);
#endif
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

void
erl_assert_error(char* expr, char* file, int line)
{   
    char message[1024];

    sprintf(message, "File %hs, line %d: %hs", file, line, expr);
    MessageBox(GetActiveWindow(), message, "Assertion failed",
	       MB_OK | MB_ICONERROR);
#if 0
    erl_crash_dump(file, line, "Assertion failed: %hs\n", expr);
#endif
    DebugBreak();
}

#endif /* DEBUG */
	    
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
	    erl_exit(-1,
		     "Windows version not supported "
		     "(min required: winnt %d.%d)\n",
		     major, minor);
    }
#else
    erl_exit(-1,
	     "Windows version not supported "
	     "(min required: win %d.%d)\n",
	     nt_major, nt_minor);
#endif
}

#ifdef USE_THREADS
static void *ethr_internal_alloc(size_t size)
{
    return erts_alloc_fnf(ERTS_ALC_T_ETHR_INTERNAL, (Uint) size);
}
static void *ethr_internal_realloc(void *ptr, size_t size)
{
    return erts_realloc_fnf(ERTS_ALC_T_ETHR_INTERNAL, ptr, (Uint) size);
}
static void ethr_internal_free(void *ptr)
{
    erts_free(ERTS_ALC_T_ETHR_INTERNAL, ptr);
}
#endif

void
erts_sys_pre_init(void)
{
    int_os_version.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    GetVersionEx(&int_os_version);
    check_supported_os_version();
#ifdef USE_THREADS
    {
	erts_thr_init_data_t eid = ERTS_THR_INIT_DATA_DEF_INITER;
	eid.alloc = ethr_internal_alloc;
	eid.realloc = ethr_internal_realloc;
	eid.free = ethr_internal_free;
	erts_thr_init(&eid);
#ifdef ERTS_ENABLE_LOCK_COUNT
	erts_lcnt_init();
#endif
    }
#endif
    erts_smp_atomic_init(&sys_misc_mem_sz, 0);
    erts_sys_env_init();
}

/*
 * the last two only used for standalone erlang
 * they should are used by sae_main in beam dll to
 * enable standalone execution via erl_api-routines
 */

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


    erts_smp_mtx_init(&sys_driver_data_lock, "sys_driver_data_lock");

#ifdef ERTS_SMP
    erts_smp_tsd_key_create(&win32_errstr_key);
#endif
    erts_smp_atomic_init(&pipe_creation_counter,0);
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
    erts_init_check_io();

    /* Suppress windows error message popups */
    SetErrorMode(SetErrorMode(0) |
		 SEM_FAILCRITICALERRORS | SEM_NOOPENFILEERRORBOX); 
}

#ifdef ERTS_SMP
void
erts_sys_schedule_interrupt(int set)
{
    erts_check_io_interrupt(set);
}

void
erts_sys_schedule_interrupt_timed(int set, long msec)
{
    erts_check_io_interrupt_timed(set, msec);
}
#endif

/*
 * Called from schedule() when it runs out of runnable processes,
 * or when Erlang code has performed INPUT_REDUCTIONS reduction
 * steps. runnable == 0 iff there are no runnable Erlang processes.
 */
void
erl_sys_schedule(int runnable)
{
#ifdef ERTS_SMP
    erts_check_io(!runnable);
    ERTS_SMP_LC_ASSERT(!ERTS_LC_IS_BLOCKING);
#else
    erts_check_io_interrupt(0);
    if (runnable) {
	erts_check_io(0);	/* Poll for I/O */
	check_async_ready();	/* Check async completions */
    } else {
	erts_check_io(check_async_ready() ? 0 : 1);
    }
#endif
}

#if defined(USE_THREADS) && !defined(ERTS_SMP)
/*
 * Async operation support.
 */

static ErlDrvEvent async_drv_event;

void
sys_async_ready(int fd)
{
    SetEvent((HANDLE)async_drv_event);
}

static int
async_drv_init(void)
{
    async_drv_event = (ErlDrvEvent) NULL;
    return 0;
}

static ErlDrvData
async_drv_start(ErlDrvPort port_num, char* name, SysDriverOpts* opts)
{
    if (async_drv_event != (ErlDrvEvent) NULL) {
	return ERL_DRV_ERROR_GENERAL;
    }
    if ((async_drv_event = (ErlDrvEvent)CreateAutoEvent(FALSE)) == (ErlDrvEvent) NULL) {
	return ERL_DRV_ERROR_GENERAL;
    }

    driver_select(port_num, async_drv_event, ERL_DRV_READ|ERL_DRV_USE, 1);
    if (init_async(async_drv_event) < 0) {
	return ERL_DRV_ERROR_GENERAL;
    }
    return (ErlDrvData)port_num;
}

static void
async_drv_stop(ErlDrvData port_num)
{
    exit_async();
    driver_select((ErlDrvPort)port_num, async_drv_event, ERL_DRV_READ|ERL_DRV_USE, 0);
    /*CloseHandle((HANDLE)async_drv_event);*/
    async_drv_event = (ErlDrvEvent) NULL;
}


static void
async_drv_input(ErlDrvData port_num, ErlDrvEvent e) 
{
    check_async_ready();

    /*
     * Our event is auto-resetting.
     */
}

#endif

