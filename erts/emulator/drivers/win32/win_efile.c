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
 * Purpose: Provides file and directory operations for Windows.
 */

#include <windows.h>
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "sys.h"
#include <ctype.h>
#include <wchar.h>
#include "erl_efile.h"

#define DBG_TRACE_MASK 0
/* 1 = file name ops
 * 2 = file descr ops
 * 4 = errors
 * 8 = path name conversion
 */
#if !DBG_TRACE_MASK
#  define DBG_TRACE(M,S)
#  define DBG_TRACE1(M,FMT,A)
#  define DBG_TRACE2(M,FMT,A,B)
#else
#  define DBG_TRACE(M,S) do { if ((M)&DBG_TRACE_MASK) fwprintf(stderr, L"DBG_TRACE %d: %s\r\n", __LINE__, (WCHAR*)(S)); }while(0)
#  define DBG_TRACE1(M,FMT,A) do { if ((M)&DBG_TRACE_MASK) fwprintf(stderr, L"DBG_TRACE %d: " L##FMT L"\r\n", __LINE__, (A)); }while(0)
#  define DBG_TRACE2(M,FMT,A,B) do { if ((M)&DBG_TRACE_MASK) fwprintf(stderr, L"DBG_TRACE %d: " L##FMT L"\r\n", __LINE__, (A), (B)); }while(0)
#endif

/*
 * Microsoft-specific function to map a WIN32 error code to a Posix errno.
 */

#define ISSLASH(a)  ((a) == L'\\' || (a) == L'/')
#define ISDIR(st) (((st).st_mode&S_IFMT) == S_IFDIR)
#define ISREG(st) (((st).st_mode&S_IFMT) == S_IFREG)

#define IS_DOT_OR_DOTDOT(s) \
    ((s)[0] == L'.' && ((s)[1] == L'\0' || ((s)[1] == L'.' && (s)[2] == L'\0')))

#define FILE_SHARE_FLAGS (FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE)

#ifndef INVALID_FILE_ATTRIBUTES
#define INVALID_FILE_ATTRIBUTES ((DWORD) 0xFFFFFFFF)
#endif

#define TICKS_PER_SECOND (10000000ULL)
#define EPOCH_DIFFERENCE (11644473600LL)

#define FILETIME_TO_EPOCH(epoch, ft) \
    do { \
	ULARGE_INTEGER ull; \
	ull.LowPart  = (ft).dwLowDateTime; \
	ull.HighPart = (ft).dwHighDateTime; \
	(epoch) = ((ull.QuadPart / TICKS_PER_SECOND) - EPOCH_DIFFERENCE); \
    } while(0)

#define EPOCH_TO_FILETIME(ft, epoch) \
    do { \
	ULARGE_INTEGER ull; \
	ull.QuadPart = (((epoch) + EPOCH_DIFFERENCE) * TICKS_PER_SECOND); \
	(ft).dwLowDateTime = ull.LowPart; \
	(ft).dwHighDateTime = ull.HighPart; \
    } while(0)


static int check_error(int result, Efile_error* errInfo);
static int set_error(Efile_error* errInfo);
static int set_os_errno(Efile_error* errInfo, DWORD os_errno);
static int is_root_unc_name(const WCHAR *path);
static int extract_root(WCHAR *name);
static unsigned short dos_to_posix_mode(int attr, const WCHAR *name);


struct wpath_tmp_buffer {
    struct wpath_tmp_buffer* next;
    WCHAR buffer[1];
};

typedef struct {
    Efile_error* errInfo;
    struct wpath_tmp_buffer* buf_list;
}Efile_call_state;

static void call_state_init(Efile_call_state* state, Efile_error* errInfo)
{
    state->errInfo = errInfo;
    state->buf_list = NULL;
}
static WCHAR* wpath_tmp_alloc(Efile_call_state* state, size_t len)
{
    size_t sz = offsetof(struct wpath_tmp_buffer, buffer)
	+ (len+1)*sizeof(WCHAR);
    struct wpath_tmp_buffer* p = driver_alloc(sz);
    p->next = state->buf_list;
    state->buf_list = p;
    return p->buffer;
}
static void call_state_free(Efile_call_state* state)
{
    while(state->buf_list) {
	struct wpath_tmp_buffer* next = state->buf_list->next;
	driver_free(state->buf_list);
	state->buf_list = next;
    }
}
static WCHAR* get_cwd_wpath_tmp(Efile_call_state* state)
{
    WCHAR dummy;
    DWORD size = GetCurrentDirectoryW(0, &dummy);
    WCHAR* ret = NULL;

    if (size) {
	ret = wpath_tmp_alloc(state, size);
	if (!GetCurrentDirectoryW(size, ret)) {
	    ret = NULL;
	}
    }
    return ret;
}
static WCHAR* get_full_wpath_tmp(Efile_call_state* state,
                                 const WCHAR* file,
				 WCHAR** file_part,
				 DWORD extra)
{
    WCHAR dummy;
    DWORD size = GetFullPathNameW(file, 0, &dummy, NULL);
    WCHAR* ret = NULL;

    if (size) {
	int ok;
	ret = wpath_tmp_alloc(state, size + extra);
	if (file_part) {
	    ok = (GetFullPathNameW(file, size, ret, file_part) != 0);
	}
	else {
	    ok = (_wfullpath(ret, file, size) != NULL);
	}
	if (!ok) {
	    ret = NULL;
	}
    }
    return ret;
}

static void ensure_wpath_max(Efile_call_state* state, WCHAR** pathp, size_t max);
static int do_rmdir(Efile_call_state*, char* name);
static int do_rename(Efile_call_state*, char* src, char* dst);
static int do_readdir(Efile_call_state*, char* name, EFILE_DIR_HANDLE*, char* buffer, size_t *size);
static int do_fileinfo(Efile_call_state*, Efile_info*, char* orig_name, int info_for_link);
static char* do_readlink(Efile_call_state*, char* name, char* buffer, size_t size);
static int do_altname(Efile_call_state*, char* orig_name, char* buffer, size_t size);


static int errno_map(DWORD last_error) {

    switch (last_error) {
    case ERROR_SUCCESS:
	return 0;
    case ERROR_INVALID_FUNCTION:
    case ERROR_INVALID_DATA:
    case ERROR_INVALID_PARAMETER:
    case ERROR_INVALID_TARGET_HANDLE:
    case ERROR_INVALID_CATEGORY:
    case ERROR_NEGATIVE_SEEK:
	return EINVAL;
    case ERROR_DIR_NOT_EMPTY:
	return EEXIST;
    case ERROR_BAD_FORMAT:
	return ENOEXEC;
    case ERROR_PATH_NOT_FOUND:
    case ERROR_FILE_NOT_FOUND:
    case ERROR_NO_MORE_FILES:
	return ENOENT;
    case ERROR_TOO_MANY_OPEN_FILES:
	return EMFILE;
    case ERROR_ACCESS_DENIED:
    case ERROR_INVALID_ACCESS:
    case ERROR_CURRENT_DIRECTORY:
    case ERROR_SHARING_VIOLATION:
    case ERROR_LOCK_VIOLATION:
    case ERROR_INVALID_PASSWORD:
    case ERROR_DRIVE_LOCKED:
	return EACCES;
    case ERROR_INVALID_HANDLE:
	return EBADF;
    case ERROR_NOT_ENOUGH_MEMORY:
    case ERROR_OUTOFMEMORY:
    case ERROR_OUT_OF_STRUCTURES:
	return ENOMEM;
    case ERROR_INVALID_DRIVE:
    case ERROR_BAD_UNIT:
    case ERROR_NOT_READY:
    case ERROR_REM_NOT_LIST:
    case ERROR_DUP_NAME:
    case ERROR_BAD_NETPATH:
    case ERROR_NETWORK_BUSY:
    case ERROR_DEV_NOT_EXIST:
    case ERROR_BAD_NET_NAME:
	return ENXIO;
    case ERROR_NOT_SAME_DEVICE:
	return EXDEV;
    case ERROR_WRITE_PROTECT:
	return EROFS;
    case ERROR_BAD_LENGTH:
    case ERROR_BUFFER_OVERFLOW:
	return E2BIG;
    case ERROR_SEEK:
    case ERROR_SECTOR_NOT_FOUND:
	return ESPIPE;
    case ERROR_NOT_DOS_DISK:
	return ENODEV;
    case ERROR_GEN_FAILURE:
	return ENODEV;
    case ERROR_SHARING_BUFFER_EXCEEDED:
    case ERROR_NO_MORE_SEARCH_HANDLES:
	return EMFILE;
    case ERROR_HANDLE_EOF:
    case ERROR_BROKEN_PIPE:
	return EPIPE;
    case ERROR_HANDLE_DISK_FULL:
    case ERROR_DISK_FULL:
	return ENOSPC;
    case ERROR_NOT_SUPPORTED:
	return ENOTSUP;
    case ERROR_FILE_EXISTS:
    case ERROR_ALREADY_EXISTS:
    case ERROR_CANNOT_MAKE:
	return EEXIST;
    case ERROR_ALREADY_ASSIGNED:
	return EBUSY;
    case ERROR_NO_PROC_SLOTS:
	return EAGAIN;
    case ERROR_CANT_RESOLVE_FILENAME:
	return EMLINK;
    case ERROR_PRIVILEGE_NOT_HELD:
	return EPERM;
    case ERROR_ARENA_TRASHED:
    case ERROR_INVALID_BLOCK:
    case ERROR_BAD_ENVIRONMENT:
    case ERROR_BAD_COMMAND:
    case ERROR_CRC:
    case ERROR_OUT_OF_PAPER:
    case ERROR_READ_FAULT:
    case ERROR_WRITE_FAULT:
    case ERROR_WRONG_DISK:
    case ERROR_NET_WRITE_FAULT:
	return EIO;
    default: /* not to do with files I expect. */
	return EIO;
    }	
}

static int
check_error(int result, Efile_error* errInfo)
{
    if (result < 0) {
	errInfo->posix_errno = errno;
	errInfo->os_errno = GetLastError();
	DBG_TRACE2(4, "ERROR os_error=%d errno=%d @@@@@@@@@@@@@@@@@@@@@@@@@@@@",
	        errInfo->os_errno, errInfo->posix_errno);
	return 0;
    }
    return 1;
}

static void
save_last_error(Efile_error* errInfo)
{
    errInfo->posix_errno = errno;
    errInfo->os_errno = GetLastError();
    DBG_TRACE2(4, "ERROR os_error=%d errno=%d $$$$$$$$$$$$$$$$$$$$$$$$$$$$$",
	errInfo->os_errno, errInfo->posix_errno);
}


/*
 * Fills the provided error information structure with information
 * with the error code given by GetLastError() and its corresponding
 * Posix error number.
 *
 * Returns 0.
 */

static int
set_error(Efile_error* errInfo)
{
    set_os_errno(errInfo, GetLastError());
    return 0;
}


static int
set_os_errno(Efile_error* errInfo, DWORD os_errno)
{
    errInfo->os_errno = os_errno;
    errInfo->posix_errno = errno_map(os_errno);
    DBG_TRACE2(4, "ERROR os_error=%d errno=%d ############################",
	    errInfo->os_errno, errInfo->posix_errno);
    return 0;
}

int
efile_init() {
   return 1;
}

/*
 * A writev with Unix semantics, but with Windows arguments 
 */
static int 
win_writev(Efile_error* errInfo,
	   HANDLE fd,                  /* handle to file */
	   FILE_SEGMENT_ELEMENT iov[], /* array of buffer pointers */
	   DWORD *size)                /* number of bytes to write */
{
    OVERLAPPED ov;
    ov.Offset = 0L;
    ov.OffsetHigh = 0L;
    ov.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
    if (ov.hEvent == NULL)
	return set_error(errInfo);
    if (! write_file_gather(fd, iov, *size, NULL, &ov))
	return set_error(errInfo);
    if (WaitForSingleObject(ov.hEvent, INFINITE) != WAIT_OBJECT_0)
	return set_error(errInfo);
    if (! GetOverlappedResult(fd, &ov, size, FALSE))
	return set_error(errInfo);
    return 1;
}


/* Check '*pathp' and convert it if needed to something that windows will accept.
 * Typically use UNC path with \\?\ prefix if absolute path is longer than 260.
 */
static void ensure_wpath(Efile_call_state* state, WCHAR** pathp)
{
    ensure_wpath_max(state, pathp, MAX_PATH);
}

static void ensure_wpath_max(Efile_call_state* state, WCHAR** pathp, size_t max)
{
    WCHAR* path = *pathp;
    WCHAR* p;
    size_t len = wcslen(path);
    int unc_fixup = 0;

    if (path[0] == 0) {
	DBG_TRACE(8, L"Let empty path pass through");
	return;
    }

    DBG_TRACE1(8,"IN: %s", path);

    if (path[1] == L':' && ISSLASH(path[2])) { /* absolute path */
	if (len >= max) {
	    WCHAR *src, *dst;

	    *pathp = wpath_tmp_alloc(state, 4+len+1);
	    dst = *pathp;
	    wcscpy(dst, L"\\\\?\\");
	    for (src=path,dst+=4; *src; src++) {
		if (*src == L'/') {
		    if (dst[-1] != L'\\') {
			*dst++ =  L'\\';
		    }
		    /*else ignore redundant slashes */
		}
		else
		    *dst++ = *src;
	    }
	    *dst = 0;
	    unc_fixup = 1;
	}
    }
    else if (!(ISSLASH(path[0]) && ISSLASH(path[1]))) { /* relative path */
	DWORD cwdLen = GetCurrentDirectoryW(0, NULL);
	DWORD absLen = cwdLen + 1 + len;
	if (absLen >= max) {
	    WCHAR *fullPath = wpath_tmp_alloc(state, 4+4+absLen);
	    DWORD fullLen;

	    fullLen = GetFullPathNameW(path, 4 + absLen, fullPath+4, NULL);
	    if (fullLen >= 4+absLen) {
		*pathp = path;
		DBG_TRACE2(8,"ensure_wpath FAILED absLen=%u %s", (int)absLen, path);
		return;
	    }
	    /* GetFullPathNameW can return paths longer than MAX_PATH without the \\?\ prefix.
	     * At least seen on Windows 7. Go figure...
	     */
	    if (fullLen >= max && wcsncmp(fullPath+4, L"\\\\?\\", 4) != 0) {
		wcsncpy(fullPath, L"\\\\?\\", 4);
		*pathp = fullPath;
	    }
	    else {
		*pathp = fullPath + 4;
	    }
	}
    }

    if (unc_fixup) {
	WCHAR* endp;

	p = *pathp;
	len = wcslen(p);
	endp = p + len;
	if (len > 4) {
	    p += 4;
	    while (*p) {
		if (p[0] == L'\\' && p[1] == L'.') {
		    if (p[2] == L'\\' || !p[2]) { /* single dot */
			wmemmove(p, p+2, (&endp[1] - &p[2]));
			endp -= 2;
		    }
		    else if (p[2] == L'.' && (p[3] == L'\\' || !p[3])) { /* double dot */
			WCHAR* r;
			for (r=p-1; *r == L'\\'; --r)
			    /*skip redundant slashes*/;
			for (; *r != L'\\'; --r)
			    /*find start of prev directory*/;
			if (r < *pathp + 6)
			    break;
			wmemmove(r, p+3, (&endp[1] - &p[3]));
			p = r;
		    }
		    else p += 3;
		}
		else ++p;
	    }
	}
    }
    DBG_TRACE1(8,"OUT: %s", *pathp);
}

int
efile_mkdir(Efile_error* errInfo,	/* Where to return error codes. */
	    char* name)			/* Name of directory to create. */
{
    Efile_call_state state;
    WCHAR* wname = (WCHAR*)name;
    int ret;

    DBG_TRACE(1, name);
    call_state_init(&state, errInfo);
    ensure_wpath_max(&state, &wname, 248); /* Yes, 248 limit for normal paths */

    ret = (int) CreateDirectoryW(wname, NULL);
    if (!ret)
	set_error(errInfo);

    call_state_free(&state);
    return ret;
}

int
efile_rmdir(Efile_error* errInfo,	/* Where to return error codes. */
	    char* name)			/* Name of directory to delete. */
{
    Efile_call_state state;
    int ret;

    DBG_TRACE(1, name);
    call_state_init(&state, errInfo);
    ret = do_rmdir(&state, name);
    call_state_free(&state);
    return ret;
}

static int do_rmdir(Efile_call_state* state, char* name)
{
    OSVERSIONINFO os;
    DWORD attr;
    WCHAR *wname = (WCHAR *) name;
    WCHAR *buffer = NULL;

    ensure_wpath(state, &wname);

    if (RemoveDirectoryW(wname) != FALSE) {
	return 1;
    }
    errno = errno_map(GetLastError());
    if (errno == EACCES) {
	attr = GetFileAttributesW(wname);
	if (attr != (DWORD) -1) {
	    if ((attr & FILE_ATTRIBUTE_DIRECTORY) == 0) {
		/* 
		 * Windows 95 reports calling RemoveDirectory on a file as an 
		 * EACCES, not an ENOTDIR.
		 */
		
		errno = ENOTDIR;
		goto end;
	    }

	    /* 
	     * Windows 95 reports removing a non-empty directory as
	     * an EACCES, not an EEXIST.  If the directory is not empty,
	     * change errno so caller knows what's going on.
	     */

	    os.dwOSVersionInfoSize = sizeof(os);
	    GetVersionEx(&os);
	    if (os.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS) {
		HANDLE handle;
		WIN32_FIND_DATAW data;
		int len = wcslen(wname);

		buffer = wpath_tmp_alloc(state, len + 4);
		wcscpy(buffer, wname);
		if (buffer[0] && buffer[len-1] != L'\\' && buffer[len-1] != L'/') {
		    wcscat(buffer, L"\\");
		}
		wcscat(buffer, L"*.*");
		handle = FindFirstFileW(buffer, &data);
		if (handle != INVALID_HANDLE_VALUE) {
		    while (1) {
			if ((wcscmp(data.cFileName, L".") != 0)
				&& (wcscmp(data.cFileName, L"..") != 0)) {
			    /*
			     * Found something in this directory.
			     */

			    errno = EEXIST;
			    break;
			}
			if (FindNextFileW(handle, &data) == FALSE) {
			    break;
			}
		    }
		    FindClose(handle);
		}
	    }
	}
    }

    if (errno == ENOTEMPTY) {
	/* 
	 * Posix allows both EEXIST or ENOTEMPTY, but we'll always
	 * return EEXIST to allow easy matching in Erlang code.
	 */

	errno = EEXIST;
    }

 end:
    save_last_error(state->errInfo);
    return 0;
}

int
efile_delete_file(Efile_error* errInfo,		/* Where to return error codes. */
		  char* name)			/* Name of file to delete. */
{
    Efile_call_state state;
    int ret;
    DBG_TRACE(1, name);
    call_state_init(&state, errInfo);
    ret = do_delete_file(&state, name);
    call_state_free(&state);
    return ret;
}

static int do_delete_file(Efile_call_state* state, char* name)
{
    DWORD attr;
    WCHAR *wname = (WCHAR *) name;

    ensure_wpath(state, &wname);

    if (DeleteFileW(wname) != FALSE) {
	return 1;
    }

    errno = errno_map(GetLastError());
    if (errno == EACCES) {
        attr = GetFileAttributesW(wname);
	if (attr != (DWORD) -1) {
	    if (attr & FILE_ATTRIBUTE_DIRECTORY) {
		/*
		 * Windows NT reports removing a directory as EACCES instead
		 * of EPERM.
		 */

		errno = EPERM;
	    }
	}
    } else if (errno == ENOENT) {
        attr = GetFileAttributesW(wname);
	if (attr != (DWORD) -1) {
	    if (attr & FILE_ATTRIBUTE_DIRECTORY) {
	    	/*
		 * Windows 95 reports removing a directory as ENOENT instead 
		 * of EPERM.
		 */

		errno = EPERM;
	    }
	}
    } else if (errno == EINVAL) {
	/*
	 * Windows NT reports removing a char device as EINVAL instead of
	 * EACCES.
	 */
	
	errno = EACCES;
    }

    return check_error(-1, state->errInfo);
}

/*
 *---------------------------------------------------------------------------
 *
 *      Changes the name of an existing file or directory, from src to dst.
 *	If src and dst refer to the same file or directory, does nothing
 *	and returns success.  Otherwise if dst already exists, it will be
 *	deleted and replaced by src subject to the following conditions:
 *	    If src is a directory, dst may be an empty directory.
 *	    If src is a file, dst may be a file.
 *	In any other situation where dst already exists, the rename will
 *	fail.  
 *
 *	Some possible error codes:
 *
 *	EACCES:     src or dst parent directory can't be read and/or written.
 *	EEXIST:	    dst is a non-empty directory.
 *	EINVAL:	    src is a root directory or dst is a subdirectory of src.
 *	EISDIR:	    dst is a directory, but src is not.
 *	ENOENT:	    src doesn't exist, or src or dst is "".
 *	ENOTDIR:    src is a directory, but dst is not.  
 *	EXDEV:	    src and dst are on different filesystems.
 *	
 * Side effects:
 *	The implementation of rename may allow cross-filesystem renames,
 *	but the caller should be prepared to emulate it with copy and
 *	delete if errno is EXDEV.
 *
 *---------------------------------------------------------------------------
 */

int
efile_rename(Efile_error* errInfo, char* src, char* dst)
{
    Efile_call_state state;
    int ret;
    DBG_TRACE(1, src);
    call_state_init(&state, errInfo);
    ret = do_rename(&state, src, dst);
    call_state_free(&state);
    return ret;
}

static int
do_rename(Efile_call_state* state,
	  char* src,			/* Original name. */
	  char* dst)			/* New name. */
{
    DWORD srcAttr, dstAttr;
    WCHAR *wsrc = (WCHAR *) src;
    WCHAR *wdst = (WCHAR *) dst;

    ensure_wpath(state, &wsrc);
    ensure_wpath(state, &wdst);

    if (MoveFileW(wsrc, wdst) != FALSE) {
	return 1;
    }

    errno = errno_map(GetLastError());
    srcAttr = GetFileAttributesW(wsrc);
    dstAttr = GetFileAttributesW(wdst);
    if (srcAttr == (DWORD) -1) {
	srcAttr = 0;
    }
    if (dstAttr == (DWORD) -1) {
	dstAttr = 0;
    }

    if (errno == EBADF) {
	errno = EACCES;
	return check_error(-1, state->errInfo);
    }
    if (errno == EACCES) {
	decode:
	if (srcAttr & FILE_ATTRIBUTE_DIRECTORY) {
	    WCHAR *srcPath, *dstPath;
	    WCHAR *srcRest, *dstRest;
	    int size;

	    srcPath = get_full_wpath_tmp(state, wsrc, &srcRest, 0);
	    if (!srcPath) {
		save_last_error(state->errInfo);
		return 0;
	    }

	    dstPath = get_full_wpath_tmp(state, wdst, &dstRest, 0);
	    if (!dstPath) {
		save_last_error(state->errInfo);
		return 0;
	    }

	    if (srcRest == NULL) {
		srcRest = srcPath + wcslen(srcPath);
	    }
	    if (_wcsnicmp(srcPath, dstPath, srcRest - srcPath) == 0) {
		/*
		 * Trying to move a directory into itself.
		 */

		errno = EINVAL;
	    }
	    if (extract_root(srcPath)) {
		/*
		 * Attempt to move a root directory.  Never allowed.
		 */
		errno = EINVAL;
	    }

	    (void) extract_root(dstPath);
	    if (dstPath[0] == L'\0') {
		/*
		 * The filename was invalid.  (Don't know why,
		 * but play it safe.)
		 */
		errno = EINVAL;
	    }
	    if (_wcsicmp(srcPath, dstPath) != 0) {
		/*
		 * If src is a directory and dst filesystem != src
		 * filesystem, errno should be EXDEV.  It is very
		 * important to get this behavior, so that the caller
		 * can respond to a cross filesystem rename by
		 * simulating it with copy and delete.  The MoveFile
		 * system call already handles the case of moving a
		 * *file* between filesystems.
		 */

		errno = EXDEV;
	    }
	}

	/*
	 * Other types of access failure is that dst is a read-only
	 * filesystem, that an open file referred to src or dest, or that
	 * src or dest specified the current working directory on the
	 * current filesystem.  EACCES is returned for those cases.
	 */

    } else if (errno == EEXIST) {
	/*
	 * Reports EEXIST any time the target already exists.  If it makes
	 * sense, remove the old file and try renaming again.
	 */

	if (srcAttr & FILE_ATTRIBUTE_DIRECTORY) {
	    if (dstAttr & FILE_ATTRIBUTE_DIRECTORY) {
		/*
		 * Overwrite empty dst directory with src directory.  The
		 * following call will remove an empty directory.  If it
		 * fails, it's because it wasn't empty.
		 */

		if (RemoveDirectoryW(wdst)) {
		    /*
		     * Now that that empty directory is gone, we can try
		     * renaming again.  If that fails, we'll put this empty
		     * directory back, for completeness.
		     */

		    if (MoveFileW(wsrc, wdst) != FALSE) {
			return 1;
		    }

		    /*
		     * Some new error has occurred.  Don't know what it
		     * could be, but report this one.
		     */

		    errno = errno_map(GetLastError());
		    CreateDirectoryW(wdst, NULL);
		    SetFileAttributesW(wdst, dstAttr);
		    if (errno == EACCES) {
			/*
			 * Decode the EACCES to a more meaningful error.
			 */

			goto decode;
		    }
		}
	    } else {	/* (dstAttr & FILE_ATTRIBUTE_DIRECTORY) == 0 */
		errno = ENOTDIR;
	    }
	} else {    /* (srcAttr & FILE_ATTRIBUTE_DIRECTORY) == 0 */
	    if (dstAttr & FILE_ATTRIBUTE_DIRECTORY) {
		errno = EISDIR;
	    } else {
		/*
		 * Overwrite existing file by:
		 * 
		 * 1. Rename existing file to temp name.
		 * 2. Rename old file to new name.
		 * 3. If success, delete temp file.  If failure,
		 *    put temp file back to old name.
		 */

		WCHAR *tempName;
		int result;
		WCHAR *rest;
		
		tempName = get_full_wpath_tmp(state, wdst, &rest, 14);
		if (!tempName || !rest) {
		    save_last_error(state->errInfo);
		    return 0;
		}

		*rest = L'\0';
		result = -1;
		if (GetTempFileNameW(tempName, L"erlr", 0, tempName) != 0) {
		    /*
		     * Strictly speaking, need the following DeleteFile and
		     * MoveFile to be joined as an atomic operation so no
		     * other app comes along in the meantime and creates the
		     * same temp file.
		     */
		     
		    DeleteFileW(tempName);
		    if (MoveFileW(wdst, tempName) != FALSE) {
			if (MoveFileW(wsrc, wdst) != FALSE) {
			    SetFileAttributesW(tempName, FILE_ATTRIBUTE_NORMAL);
			    DeleteFileW(tempName);
			    return 1;
			} else {
			    DeleteFileW(wdst);
			    MoveFileW(tempName, wdst);
			}
		    } 

		    /*
		     * Can't backup dst file or move src file.  Return that
		     * error.  Could happen if an open file refers to dst.
		     */

		    errno = errno_map(GetLastError());
		    if (errno == EACCES) {
			/*
			 * Decode the EACCES to a more meaningful error.
			 */
			goto decode;
		    }
		}
		return result;
	    }
	}
    }
    return check_error(-1, state->errInfo);
}

int
efile_chdir(Efile_error* errInfo,	/* Where to return error codes. */
	    char* name)			/* Name of directory to make current. */
{
    /* We don't even try to handle long paths here
     * as current working directory is always limited to MAX_PATH
     * even if we use UNC paths and SetCurrentDirectoryW()
     */
    int success = check_error(_wchdir((WCHAR *) name), errInfo);
    if (!success && errInfo->posix_errno == EINVAL)
	/* POSIXification of errno */
	errInfo->posix_errno = ENOENT;
    return success;
}

int
efile_getdcwd(Efile_error* errInfo,		/* Where to return error codes. */
	      int drive,			/* 0 - current, 1 - A, 2 - B etc. */
	      char* buffer,			/* Where to return the current directory. */
	      size_t size)			/* Size of buffer. */
{
    WCHAR *wbuffer = (WCHAR *) buffer;
    size_t wbuffer_size = size / 2; 
    DBG_TRACE(1, L"#getdcwd#");
    if (_wgetdcwd(drive, wbuffer, wbuffer_size) == NULL) {
	return check_error(-1, errInfo);
    }
    DBG_TRACE1(8, "getdcwd OS=%s", wbuffer);
    if (wcsncmp(wbuffer, L"\\\\?\\", 4) == 0) {
	wmemmove(wbuffer, wbuffer+4, wcslen(wbuffer+4)+1);
    }
    for ( ; *wbuffer; wbuffer++) 
	if (*wbuffer == L'\\')
	    *wbuffer = L'/';
    DBG_TRACE1(8, "getdcwd ERLANG=%s", (WCHAR*)buffer);
    return 1;
}

int
efile_readdir(Efile_error* errInfo, char* name, EFILE_DIR_HANDLE* dir_handle,
	      char* buffer, size_t *size)
{
    Efile_call_state state;
    int ret;
    DBG_TRACE(dir_handle?2:1, name);
    call_state_init(&state, errInfo);
    ret = do_readdir(&state, name, dir_handle, buffer, size);
    call_state_free(&state);
    return ret;
}

static int do_readdir(Efile_call_state* state,
                      char* name,                   /* Name of directory to list */
		      EFILE_DIR_HANDLE* dir_handle, /* Handle of opened directory or NULL */
		      char* buffer,                 /* Buffer to put one filename in */
		      size_t *size)                 /* in-out size of buffer/size of filename excluding zero
					               termination in bytes*/
{
    HANDLE dir;			/* Handle to directory. */
    WIN32_FIND_DATAW findData;	/* Data found by FindFirstFile() or FindNext(). */
    /* Alignment is not honored, this works on x86 because of alignment fixup by processor.
       Not perfect, but faster than alinging by hand (really) */
    WCHAR *wbuffer = (WCHAR *) buffer;

    /*
     * First time we must setup everything.
     */

    if (*dir_handle == NULL) {
	WCHAR *wname = (WCHAR *) name;
	WCHAR* wildcard;
	int length;
	WCHAR* s;

	ensure_wpath_max(state, &wname, MAX_PATH-2);
	length = wcslen(wname);

	wildcard = wpath_tmp_alloc(state, length+3);

	wcscpy(wildcard, wname);
	s = wildcard+length-1;
	if (*s != L'/' && *s != L'\\')
	    *++s = L'\\';
	*++s = L'*';
	*++s = L'\0';
	DEBUGF(("Reading %ws\n", wildcard));
	dir = FindFirstFileW(wildcard, &findData);
	if (dir == INVALID_HANDLE_VALUE) {
	    set_error(state->errInfo);
	    return 0;
	}
	*dir_handle = (EFILE_DIR_HANDLE) dir;

	if (!IS_DOT_OR_DOTDOT(findData.cFileName)) {
	    wcscpy(wbuffer, findData.cFileName);
	    *size = wcslen(wbuffer)*2;
	    return 1;
	}
    }

    /*
     * Retrieve the name of the next file using the directory handle.
     */

    dir = (HANDLE) *dir_handle;

    for (;;) {
	if (FindNextFileW(dir, &findData)) {
	    if (IS_DOT_OR_DOTDOT(findData.cFileName))
		continue;
	    wcscpy(wbuffer, findData.cFileName);
	    *size = wcslen(wbuffer)*2;
	    return 1;
	}

	if (GetLastError() == ERROR_NO_MORE_FILES) {
	    state->errInfo->posix_errno = state->errInfo->os_errno = 0;
	}
	else {
	    set_error(state->errInfo);
	}
	FindClose(dir);
	return 0;
    }
}

int
efile_openfile(Efile_error* errInfo, char* name, int flags, int* pfd, Sint64* pSize)
{
    Efile_call_state state;
    int ret;
    DBG_TRACE1(1, "openfile(%s)", name);
    call_state_init(&state, errInfo);
    ret = do_openfile(&state, name, flags, pfd, pSize);
    call_state_free(&state);
    return ret;
}

static
int do_openfile(Efile_call_state* state,        /* Where to return error codes. */
	        char* name,			/* Name of directory to open. */
	        int flags,			/* Flags to use for opening. */
	        int* pfd,			/* Where to store the file descriptor. */
	        Sint64* pSize)			/* Where to store the size of the file. */
{
    Efile_error* errInfo = state->errInfo;
    BY_HANDLE_FILE_INFORMATION fileInfo; /* File information from a handle. */
    HANDLE fd;			/* Handle to open file. */
    DWORD access;		/* Access mode: GENERIC_READ, GENERIC_WRITE. */
    DWORD crFlags;
    DWORD flagsAndAttrs = FILE_ATTRIBUTE_NORMAL;
    WCHAR *wname = (WCHAR *) name;

    switch (flags & (EFILE_MODE_READ|EFILE_MODE_WRITE)) {
    case EFILE_MODE_READ:
	access = GENERIC_READ;
	crFlags = OPEN_EXISTING;
	break;
    case EFILE_MODE_WRITE:
	access = GENERIC_WRITE;
	crFlags = CREATE_ALWAYS;
	break;
    case EFILE_MODE_READ_WRITE:
	access = GENERIC_READ|GENERIC_WRITE;
	crFlags = OPEN_ALWAYS;
	break;
    default:
	errno = EINVAL;
	check_error(-1, errInfo);
	return 0;
    }

    if (flags & EFILE_MODE_SYNC) {
        flagsAndAttrs = FILE_FLAG_WRITE_THROUGH;
    }

    if (flags & EFILE_MODE_APPEND) {
	crFlags = OPEN_ALWAYS;
    }
    if (flags & EFILE_MODE_EXCL) {
	crFlags = CREATE_NEW;
    }
    ensure_wpath(state, &wname);
    fd = CreateFileW(wname, access,
		    FILE_SHARE_FLAGS,
		    NULL, crFlags, flagsAndAttrs, NULL);

    /*
     * Check for errors.
     */

    if (fd == INVALID_HANDLE_VALUE) {
	DWORD attr;

	set_error(errInfo);

	/*
	 * If the error is EACESS, the reason could be that we tried to
	 * open a directory.  In that case, we'll change the error code
	 * to EISDIR.
	 */
	if (errInfo->posix_errno && 
	    (attr = GetFileAttributesW(wname)) != INVALID_FILE_ATTRIBUTES && 
	    (attr & FILE_ATTRIBUTE_DIRECTORY)) {
	    errInfo->posix_errno = EISDIR;
	}
	return 0;
    }

    /*
     * Get and return the length of the open file.
     */
       
    if (!GetFileInformationByHandle(fd, &fileInfo))
	return set_error(errInfo);
    *pfd = (int) fd;
    if (pSize) {
	*pSize = (Sint64)
	    (((Uint64)fileInfo.nFileSizeHigh << 32) | 
	     (Uint64)fileInfo.nFileSizeLow);
    }
    return 1;
}

int 
efile_may_openfile(Efile_error* errInfo, char *name)
{
    Efile_call_state state;
    WCHAR *wname = (WCHAR *) name;
    DWORD attr;
    int ret;

    DBG_TRACE(1, name);
    call_state_init(&state, errInfo);
    ensure_wpath(&state, &wname);
    if ((attr = GetFileAttributesW(wname)) == INVALID_FILE_ATTRIBUTES) {
	errno = ENOENT;
	ret = check_error(-1, errInfo);
    }
    else if (attr & FILE_ATTRIBUTE_DIRECTORY) {
	errno = EISDIR;
	ret = check_error(-1, errInfo);
    }
    else ret = 1;

    call_state_free(&state);
    return ret;
}

void
efile_closefile(fd)
int fd;				/* File descriptor for file to close. */
{
    DBG_TRACE(2, L"");
    CloseHandle((HANDLE) fd);
}

FILE* efile_wfopen(const WCHAR* name, const WCHAR* mode)
{
    Efile_call_state state;
    Efile_error dummy;
    FILE* f;
    call_state_init(&state, &dummy);
    ensure_wpath(&state, (WCHAR**)&name);
    f = _wfopen(name, mode);
    call_state_free(&state);
    return f;
}

int
efile_fdatasync(errInfo, fd)
Efile_error* errInfo;		/* Where to return error codes. */
int fd;				/* File descriptor for file to sync. */
{
    DBG_TRACE(2, L"");
    /* Not available in Windows, just call regular fsync */
    return efile_fsync(errInfo, fd);
}

int
efile_fsync(errInfo, fd)
Efile_error* errInfo;		/* Where to return error codes. */
int fd;				/* File descriptor for file to sync. */
{
    DBG_TRACE(2, L"");
    if (!FlushFileBuffers((HANDLE) fd)) {
	return check_error(-1, errInfo);
    }
    return 1;
}

int
efile_fileinfo(Efile_error* errInfo, Efile_info* pInfo,
	       char* orig_name, int info_for_link)
{
    Efile_call_state state;
    int ret;
    DBG_TRACE(1, L"");
    call_state_init(&state, errInfo);
    ret = do_fileinfo(&state, pInfo, orig_name, info_for_link);
    call_state_free(&state);
    return ret;
}

static int
do_fileinfo(Efile_call_state* state, Efile_info* pInfo,
	    char* orig_name, int info_for_link)
{
    Efile_error* errInfo = state->errInfo;
    HANDLE findhandle;		/* Handle returned by FindFirstFile(). */
    WIN32_FIND_DATAW findbuf;	/* Data return by FindFirstFile(). */
    WCHAR* name = NULL;
    WCHAR* win_path;
    int name_len;
    int drive;			/* Drive for filename (1 = A:, 2 = B: etc). */
    WCHAR *worig_name = (WCHAR *) orig_name;

    ensure_wpath(state, &worig_name);
    /* Don't allow wildcards to be interpreted by system */


    /*
     * Move the name to a buffer and make sure to remove a trailing
     * slash, because it causes FindFirstFile() to fail on Win95.
     */

    name_len = wcslen(worig_name);

    name = wpath_tmp_alloc(state, name_len+1);
    wcscpy(name, worig_name);
    if (name_len > 2 && ISSLASH(name[name_len-1]) &&
	name[name_len-2] != L':') {
	name[name_len-1] = L'\0';
    }

    win_path = name;
    if (wcsncmp(name, L"\\\\?\\", 4) == 0) {
	win_path += 4;
    }

    if (wcspbrk(win_path, L"?*")) {
    enoent:
	errInfo->posix_errno = ENOENT;
	errInfo->os_errno = ERROR_FILE_NOT_FOUND;
        return 0;
    }

    /* Try to get disk from name.  If none, get current disk.  */

    if (win_path[1] != L':') {
	WCHAR* cwd_path = get_cwd_wpath_tmp(state);
        drive = 0;
	if (cwd_path[1] == L':') {
	    drive = towlower(cwd_path[0]) - L'a' + 1;
	}
    } else if (*win_path && win_path[2] == L'\0') {
	/*
	 * X: and nothing more is an error.
	 */
	errInfo->posix_errno = ENOENT;
	errInfo->os_errno = ERROR_FILE_NOT_FOUND;
	return 0;
    } else {
        drive = towlower(*win_path) - L'a' + 1;
    }

    findhandle = FindFirstFileW(name, &findbuf);
    if (findhandle == INVALID_HANDLE_VALUE) {
	WCHAR* path = NULL;

        if (!(wcspbrk(name, L"./\\") &&
	      (path = get_full_wpath_tmp(state, name, NULL, 0)) &&
	      /* root dir. ('C:\') or UNC root dir. ('\\server\share\') */
	      ((wcslen(path) == 3) || is_root_unc_name(path)) &&
	      (GetDriveTypeW(path) > 1)   ) ) {

	    errInfo->posix_errno = ENOENT;
	    errInfo->os_errno = ERROR_FILE_NOT_FOUND;
	    return 0;
	}

        /*
         * Root directories (such as C:\ or \\server\share\ are fabricated.
         */
	
        findbuf.dwFileAttributes = FILE_ATTRIBUTE_DIRECTORY;
        findbuf.nFileSizeHigh = 0;
        findbuf.nFileSizeLow = 0;
        findbuf.cFileName[0] = L'\0';

	pInfo->links = 1;
	pInfo->cTime = pInfo->accessTime = pInfo->modifyTime = 0;
    } else {
	SYSTEMTIME SystemTime;
        FILETIME LocalFTime;

	/*first check if we are a symlink */
	if (!info_for_link && (findbuf.dwFileAttributes &
			       FILE_ATTRIBUTE_REPARSE_POINT)){
	    /*
	     * given that we know this is a symlink,
	     we should be able to find its target */
	    WCHAR* target_name = (WCHAR*) do_readlink(state, (char *) name, NULL, 0);
	    if (target_name) {
		FindClose(findhandle);
		return do_fileinfo(state, pInfo,
				   (char *) target_name, info_for_link);
	    }
	}

	/* number of links: */
	{
	    HANDLE handle;	/* Handle returned by CreateFile() */
	    BY_HANDLE_FILE_INFORMATION fileInfo; /* from  CreateFile() */

            /* We initialise nNumberOfLinks as GetFileInformationByHandle
               does not always initialise this field */
            fileInfo.nNumberOfLinks = 1;
	    if (handle = CreateFileW(name, GENERIC_READ, FILE_SHARE_FLAGS, NULL,
				    OPEN_EXISTING, 0, NULL)) {
		GetFileInformationByHandle(handle, &fileInfo);
		pInfo->links = fileInfo.nNumberOfLinks;
		CloseHandle(handle);
	    } else {
		pInfo->links = 1;
	    }	
	}

        FILETIME_TO_EPOCH(pInfo->modifyTime, findbuf.ftLastWriteTime);

        if (findbuf.ftLastAccessTime.dwLowDateTime == 0 &&
	    findbuf.ftLastAccessTime.dwHighDateTime == 0) {
	    pInfo->accessTime = pInfo->modifyTime;
	} else {
	    FILETIME_TO_EPOCH(pInfo->accessTime, findbuf.ftLastAccessTime);
	}

        if (findbuf.ftCreationTime.dwLowDateTime == 0 &&
	    findbuf.ftCreationTime.dwHighDateTime == 0) {
	    pInfo->cTime = pInfo->modifyTime;
	} else {
	    FILETIME_TO_EPOCH(pInfo->cTime ,findbuf.ftCreationTime);
	}
        FindClose(findhandle);
    }

    pInfo->size_low = findbuf.nFileSizeLow;
    pInfo->size_high = findbuf.nFileSizeHigh;
	
    if (info_for_link && (findbuf.dwFileAttributes &
			  FILE_ATTRIBUTE_REPARSE_POINT))
	pInfo->type = FT_SYMLINK;
    else if (findbuf.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
	pInfo->type = FT_DIRECTORY;
    else
	pInfo->type = FT_REGULAR;

    if (findbuf.dwFileAttributes & FILE_ATTRIBUTE_READONLY)
	pInfo->access = FA_READ;
    else
	pInfo->access = FA_READ|FA_WRITE;

    pInfo->mode = dos_to_posix_mode(findbuf.dwFileAttributes, name);
    pInfo->major_device = drive;
    pInfo->minor_device = 0;
    pInfo->inode = 0;
    pInfo->uid = 0;
    pInfo->gid = 0;
    
    return 1;
}

int
efile_write_info(Efile_error* errInfo,
		 Efile_info* pInfo,
		 char* name)
{
    Efile_call_state state;
    int ret;
    call_state_init(&state, errInfo);
    ret = do_write_info(&state, pInfo, name);
    call_state_free(&state);
    return ret;
}

static int
do_write_info(Efile_call_state* state,
              Efile_info* pInfo,
	      char* name)
{
    Efile_error* errInfo = state->errInfo;
    SYSTEMTIME timebuf;
    FILETIME ModifyFileTime;
    FILETIME AccessFileTime;
    FILETIME CreationFileTime;
    HANDLE fd;
    DWORD attr;
    DWORD tempAttr;
    WCHAR *wname = (WCHAR *) name;

    DBG_TRACE(1, name);

    ensure_wpath(state, &wname);

    /*
     * Get the attributes for the file.
     */

    tempAttr = attr = GetFileAttributesW(wname);
    if (attr == 0xffffffff) {
	return set_error(errInfo);
    }
    if (pInfo->mode != -1) {
	if (pInfo->mode & _S_IWRITE) {
	    /* clear read only bit */
	    attr &= ~FILE_ATTRIBUTE_READONLY;
	} else {
	    /* set read only bit */
	    attr |= FILE_ATTRIBUTE_READONLY;
	}
    }

    /*
     * Construct all file times.
     */

    EPOCH_TO_FILETIME(ModifyFileTime,   pInfo->modifyTime);
    EPOCH_TO_FILETIME(AccessFileTime,   pInfo->accessTime);
    EPOCH_TO_FILETIME(CreationFileTime, pInfo->cTime);

    /*
     * If necessary, set the file times.
     */

    /*
     * If the has read only access, we must temporarily turn on
     * write access (this is necessary for native filesystems,
     * but not for NFS filesystems).
     */

    if (tempAttr & FILE_ATTRIBUTE_READONLY) {
	tempAttr &= ~FILE_ATTRIBUTE_READONLY;
	if (!SetFileAttributesW(wname, tempAttr)) {
	    return set_error(errInfo);
	}
    }

    fd = CreateFileW(wname, GENERIC_READ|GENERIC_WRITE,
	    FILE_SHARE_FLAGS,
	    NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (fd != INVALID_HANDLE_VALUE) {
	BOOL result = SetFileTime(fd, &CreationFileTime, &AccessFileTime, &ModifyFileTime);
	if (!result) {
	    return set_error(errInfo);
	}
	CloseHandle(fd);
    }

    /*
     * If the file doesn't have the correct attributes, set them now.
     * (It could have been done before setting the file times, above).
     */

    if (tempAttr != attr) {
	if (!SetFileAttributesW(wname, attr)) {
	    return set_error(errInfo);
	}
    }
    return 1;
}


int
efile_pwrite(errInfo, fd, buf, count, offset)
Efile_error* errInfo;		/* Where to return error codes. */
int fd;				/* File descriptor to write to. */
char* buf;			/* Buffer to write. */
size_t count;			/* Number of bytes to write. */
Sint64 offset;			/* where to write it */
{
    int res;
    DBG_TRACE(2, L"");
    res = efile_seek(errInfo, fd, offset, EFILE_SEEK_SET, NULL);
    if (res) {
	return efile_write(errInfo, EFILE_MODE_WRITE, fd, buf, count);
    } else {
	return res;
    }
}

/* position and read/write as a single atomic op */
int
efile_pread(errInfo, fd, offset, buf, count, pBytesRead)
Efile_error* errInfo;		/* Where to return error codes. */
int fd;				/* File descriptor to read from. */
Sint64 offset;			/* Offset in bytes from BOF. */
char* buf;			/* Buffer to read into. */
size_t count;			/* Number of bytes to read. */
size_t* pBytesRead;		/* Where to return number of bytes read. */
{
    int res;
    DBG_TRACE(2, L"");
    res = efile_seek(errInfo, fd, offset, EFILE_SEEK_SET, NULL);
    if (res) {
	return efile_read(errInfo, EFILE_MODE_READ, fd, buf, count, pBytesRead);
    } else {
	return res;
    }
}



int
efile_write(errInfo, flags, fd, buf, count)
Efile_error* errInfo;		/* Where to return error codes. */
int flags;			/* Flags given when file was opened. */
int fd;				/* File descriptor to write to. */
char* buf;			/* Buffer to write. */
size_t count;			/* Number of bytes to write. */
{
    DWORD written;		/* Bytes written in last operation. */
    OVERLAPPED overlapped;
    OVERLAPPED* pOverlapped = NULL;

    DBG_TRACE(2, L"");
    if (flags & EFILE_MODE_APPEND) {
	memset(&overlapped, 0, sizeof(overlapped));
	overlapped.Offset = 0xffffffff;
	overlapped.OffsetHigh = 0xffffffff;
	pOverlapped = &overlapped;
    }
    while (count > 0) {
	if (!WriteFile((HANDLE) fd, buf, count, &written, pOverlapped))
	    return set_error(errInfo);
	buf += written;
	count -= written;
    }
    return 1;
}

int
efile_writev(Efile_error* errInfo,   /* Where to return error codes */
	     int flags,              /* Flags given when file was
				      * opened */
	     int fd,                 /* File descriptor to write to */
	     SysIOVec* iov,          /* Vector of buffer structs.
				      * The structs are unchanged 
				      * after the call */
	     int iovcnt)             /* Number of structs in vector */
{
    int cnt;                         /* Buffers so far written */
    OVERLAPPED overlapped;
    OVERLAPPED* pOverlapped = NULL;

    DBG_TRACE(2, L"");
    ASSERT(iovcnt >= 0);
    
    if (flags & EFILE_MODE_APPEND) {
	memset(&overlapped, 0, sizeof(overlapped));
	overlapped.Offset = 0xffffffff;
	overlapped.OffsetHigh = 0xffffffff;
	pOverlapped = &overlapped;
    }
    for (cnt = 0; cnt < iovcnt; cnt++) {
	if (iov[cnt].iov_base && iov[cnt].iov_len > 0) {
	    /* Non-empty buffer */
	    int p;                   /* Position in buffer */
	    int w = iov[cnt].iov_len;/* Bytes written in this call */
	    for (p = 0; p < iov[cnt].iov_len; p += w) {
		if (!WriteFile((HANDLE) fd, 
			       iov[cnt].iov_base + p, 
			       iov[cnt].iov_len - p, 
			       &w, 
			       pOverlapped))
		    return set_error(errInfo);
	    }
	}
    }	
    return 1;
}

int
efile_read(errInfo, flags, fd, buf, count, pBytesRead)
Efile_error* errInfo;		/* Where to return error codes. */
int flags;			/* Flags given when file was opened. */
int fd;				/* File descriptor to read from. */
char* buf;			/* Buffer to read into. */
size_t count;			/* Number of bytes to read. */
size_t* pBytesRead;		/* Where to return number of bytes read. */
{
    DWORD nbytes = 0;

    DBG_TRACE(2, L"");
    if (!ReadFile((HANDLE) fd, buf, count, &nbytes, NULL))
	return set_error(errInfo);

    *pBytesRead = nbytes;
    return 1;
}

int
efile_seek(errInfo, fd, offset, origin, new_location)
Efile_error* errInfo;		/* Where to return error codes. */
int fd;				/* File descriptor to do the seek on. */
Sint64 offset;			/* Offset in bytes from the given origin. */
int origin;			/* Origin of seek (SEEK_SET, SEEK_CUR,
				 * SEEK_END).
				 */
Sint64* new_location;		/* Resulting new location in file. */
{
    LARGE_INTEGER off, new_loc;
    
    DBG_TRACE(2, L"");
    switch (origin) {
    case EFILE_SEEK_SET: origin = FILE_BEGIN; break;
    case EFILE_SEEK_CUR: origin = FILE_CURRENT; break;
    case EFILE_SEEK_END: origin = FILE_END; break;
    default:
	errno = EINVAL;
	check_error(-1, errInfo);
	break;
    }
    
    off.QuadPart = offset;
    if (! SetFilePointerEx((HANDLE) fd, off,
	new_location ? &new_loc : NULL, origin)) {
	return set_error(errInfo);
    }
    if (new_location) {
	*new_location = new_loc.QuadPart;
	DEBUGF(("efile_seek(offset=%ld, origin=%d) -> %ld\n", 
		(long) offset, origin, (long) *new_location));
    } else {
	DEBUGF(("efile_seek(offset=%ld, origin=%d)\n", (long) offset, origin));
    }
    return 1;
}

int
efile_truncate_file(errInfo, fd, flags)
Efile_error* errInfo;		/* Where to return error codes. */
int *fd;				/* File descriptor for file to truncate. */
int flags;
{
    DBG_TRACE(2, L"");
    if (!SetEndOfFile((HANDLE) (*fd)))
	return set_error(errInfo);
    return 1;
}


/*
 * is_root_unc_name - returns TRUE if the argument is a UNC name specifying
 *      a root share.  That is, if it is of the form \\server\share\.
 *      This routine will also return true if the argument is of the
 *      form \\server\share (no trailing slash) but Win32 currently
 *      does not like that form.
 *
 *      Forward slashes ('/') may be used instead of backslashes ('\').
 */

static int
is_root_unc_name(const WCHAR *path)
{
    /*
     * If a root UNC name, path will start with 2 (but not 3) slashes
     */

    if ((wcslen(path) >= 5) /* minimum string is "//x/y" */
	&& ISSLASH(path[0]) && ISSLASH(path[1]))
    {
        const WCHAR *p = path + 2;

        /*
         * find the slash between the server name and share name
         */
        while ( * ++ p )
            if ( ISSLASH(*p) )
                break ;

        if ( *p && p[1] )
        {
            /*
             * is there a further slash?
             */
            while ( * ++ p )
                if ( ISSLASH(*p) )
                    break ;

            /*
             * just final slash (or no final slash)
             */
            if ( !*p || !p[1])
                return 1;
        }
    }

    return 0 ;
}

/*
 * Extracts the root part of an absolute filename (by modifying the string
 * pointed to by the name argument).  The name can start
 * with either a driver letter (for example, C:\), or a UNC name
 * (for example, \\guinness\bjorn).
 *
 * If the name is invalid, the buffer will be modified to point to
 * an empty string.
 *
 * Returns: 1 if the name consists of just the root part, 0 if
 * 	    the name was longer.
 */

static int
extract_root(WCHAR* name)
{
    int len = wcslen(name);

    if (iswalpha(name[0]) && name[1] == L':' && ISSLASH(name[2])) {
	WCHAR c = name[3];
	name[3] = L'\0';
	return c == L'\0';
    } else if (len < 5 || !ISSLASH(name[0]) || !ISSLASH(name[1])) {
	goto error;
    } else {			/* Try to find the end of the UNC name. */
	WCHAR* p;
	WCHAR c;

        /*
         * Find the slash between the server name and share name.
         */

	for (p = name + 2; *p; p++)
            if (ISSLASH(*p))
                break;
	if (*p == L'\0')
	    goto error;

	/*
	 * Find the slash after the share name.
	 */

	for (p++; *p; p++)
            if (ISSLASH(*p))
                break;
	c = *p;
	*p = L'\0';
	return c == L'\0' || p[1] == L'\0';
    }

 error:
    *name = L'\0';
    return 1;
}

static unsigned short
dos_to_posix_mode(int attr, const WCHAR *name)
{
    register unsigned short uxmode;
    unsigned dosmode;
    register const WCHAR *p;

    dosmode = attr & 0xff;
    if ((p = name)[1] == L':')
	p += 2;

    /* check to see if this is a directory - note we must make a special
     * check for the root, which DOS thinks is not a directory
     */

    uxmode = (unsigned short)
	(((ISSLASH(*p) && !p[1]) || (dosmode & FILE_ATTRIBUTE_DIRECTORY) ||
	       *p == L'\0') ? _S_IFDIR|_S_IEXEC : _S_IFREG);

    /* If attribute byte does not have read-only bit, it is read-write */

    uxmode |= (dosmode & FILE_ATTRIBUTE_READONLY) ?
	_S_IREAD : (_S_IREAD|_S_IWRITE);

    /* see if file appears to be executable - check extension of name */

    if (p = wcsrchr(name, L'.')) {
        if (!_wcsicmp(p, L".exe") ||
	    !_wcsicmp(p, L".cmd") ||
	    !_wcsicmp(p, L".bat") ||
	    !_wcsicmp(p, L".com"))
            uxmode |= _S_IEXEC;
    }

    /* propagate user read/write/execute bits to group/other fields */

    uxmode |= (uxmode & 0700) >> 3;
    uxmode |= (uxmode & 0700) >> 6;

    return uxmode;
}


int
efile_readlink(Efile_error* errInfo, char* name, char* buffer, size_t size)
{
    Efile_call_state state;
    int ret;
    DBG_TRACE(1, name);
    call_state_init(&state, errInfo);
    ret = !!do_readlink(&state, name, buffer, size);
    call_state_free(&state);
    return ret;
}

/* If buffer==0, return buffer allocated by wpath_tmp_allocate
*/
static char*
do_readlink(Efile_call_state* state, char* name, char* buffer, size_t size)
{
    /*
     * load dll and see if we have CreateSymbolicLink at runtime:
     * (Vista only)
     */
    HINSTANCE hModule = NULL;
    WCHAR *wname = (WCHAR *) name;
    WCHAR *wbuffer = (WCHAR *) buffer;
    DWORD wsize = size / sizeof(WCHAR);
    char* ret = NULL;

    if ((hModule = LoadLibrary("kernel32.dll")) != NULL) {
	typedef DWORD (WINAPI * GETFINALPATHNAMEBYHANDLEPTR)(
							     HANDLE hFile,
							     LPCWSTR lpFilePath,
							     DWORD cchFilePath,
							     DWORD dwFlags);

	GETFINALPATHNAMEBYHANDLEPTR pGetFinalPathNameByHandle =
	    (GETFINALPATHNAMEBYHANDLEPTR)GetProcAddress(hModule, "GetFinalPathNameByHandleW");

	if (pGetFinalPathNameByHandle != NULL) {
	    DWORD fileAttributes;
	    ensure_wpath(state, &wname);
	    /* first check if file is a symlink; {error, einval} otherwise */
	    fileAttributes = GetFileAttributesW(wname);
	    if ((fileAttributes & FILE_ATTRIBUTE_REPARSE_POINT)) {
		DWORD success = 0;
		HANDLE h = CreateFileW(wname, GENERIC_READ, FILE_SHARE_FLAGS, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
		int len;
		if(h != INVALID_HANDLE_VALUE) {
		    if (!wbuffer) { /* dynamic allocation */
			WCHAR dummy;
			wsize = pGetFinalPathNameByHandle(h, &dummy, 0, 0);
			if (wsize) {
			    wbuffer = wpath_tmp_alloc(state, wsize);
			}
		    }
		    if (wbuffer
			&& (success = pGetFinalPathNameByHandle(h, wbuffer, wsize, 0))
			&& success < wsize) {
			WCHAR* wp;

			/* GetFinalPathNameByHandle prepends path with "\\?\": */
			len = wcslen(wbuffer);
			wmemmove(wbuffer,wbuffer+4,len-3);
			if (len - 4 >= 2 && wbuffer[1] == L':' && wbuffer[0] >= L'A' &&
			    wbuffer[0] <= L'Z') {
			    wbuffer[0] = wbuffer[0] + L'a' - L'A';
			}
			
			for (wp=wbuffer ; *wp; wp++)
			    if (*wp == L'\\')
				*wp = L'/';
		    }
		    CloseHandle(h);
		}
		if (success) {
		    ret = (char*) wbuffer;
		} else {
		    set_error(state->errInfo);
		}
	    } else {
		errno = EINVAL;
		save_last_error(state->errInfo);
	    }
	    goto done;
	}
    }
    errno = ENOTSUP;
    save_last_error(state->errInfo);

done:
    if (hModule)
	FreeLibrary(hModule);
    return ret;
}


int
efile_altname(Efile_error* errInfo, char* orig_name, char* buffer, size_t size)
{
    Efile_call_state state;
    int ret;
    DBG_TRACE(1, orig_name);
    call_state_init(&state, errInfo);
    ret = do_altname(&state, orig_name, buffer, size);
    call_state_free(&state);
    return ret;
}

static int
do_altname(Efile_call_state* state, char* orig_name, char* buffer, size_t size)
{
    WIN32_FIND_DATAW wfd;
    HANDLE fh;
    WCHAR* name;
    int name_len;
    WCHAR* full_path = NULL;
    WCHAR *worig_name = (WCHAR *) orig_name;
    WCHAR *wbuffer = (WCHAR *) buffer;
    int drive;			/* Drive for filename (1 = A:, 2 = B: etc). */

    /* Don't allow wildcards to be interpreted by system */

    if (wcspbrk(worig_name, L"?*")) {
    enoent:
	state->errInfo->posix_errno = ENOENT;
	state->errInfo->os_errno = ERROR_FILE_NOT_FOUND;
        return 0;
    }

    /*
     * Move the name to a buffer and make sure to remove a trailing
     * slash, because it causes FindFirstFile() to fail on Win95.
     */
    ensure_wpath(state, &worig_name);
    name_len = wcslen(worig_name);

    name = wpath_tmp_alloc(state, name_len + 1);
    wcscpy(name, worig_name);
    if (name_len > 2 && ISSLASH(name[name_len-1]) &&
	name[name_len-2] != L':') {
	name[name_len-1] = L'\0';
    }
    
    /* Try to get disk from name.  If none, get current disk.  */

    if (name[1] != L':') {
	WCHAR* cwd_path = get_cwd_wpath_tmp(state);
        drive = 0;
        if (cwd_path[1] == L':') {
	    drive = towlower(cwd_path[0]) - L'a' + 1;
	}
    } else if (*name && name[2] == L'\0') {
	/*
	 * X: and nothing more is an error.
	 */
	goto enoent;
    } else {
        drive = towlower(*name) - L'a' + 1;
    }
    fh = FindFirstFileW(name,&wfd);
    if (fh == INVALID_HANDLE_VALUE) {
	DWORD fff_error = GetLastError();
        if (!(wcspbrk(name, L"./\\") &&
	      (full_path = get_full_wpath_tmp(state, name, NULL, 0)) &&
	      /* root dir. ('C:\') or UNC root dir. ('\\server\share\') */
	      ((wcslen(full_path) == 3) || is_root_unc_name(full_path)) &&
	      (GetDriveTypeW(full_path) > 1)   ) ) {

	    set_os_errno(state->errInfo, fff_error);
	    return 0;
	}
        /*
         * Root directories (such as C:\ or \\server\share\ are fabricated.
         */
	wcscpy(wbuffer,name);
	return 1;
    }
	
    wcscpy(wbuffer,wfd.cAlternateFileName);
    if (!*wbuffer) {
	wcscpy(wbuffer,wfd.cFileName);
    }
    FindClose(fh);
    return 1;
}


int
efile_link(Efile_error* errInfo, char* old, char* new)
{
    Efile_call_state state;
    WCHAR *wold = (WCHAR *) old;
    WCHAR *wnew = (WCHAR *) new;
    int ret;
    DBG_TRACE(1, old);
    call_state_init(&state, errInfo);
    ensure_wpath(&state, &wold);
    ensure_wpath(&state, &wnew);
    if(!CreateHardLinkW(wnew, wold, NULL)) {
	ret = set_error(errInfo);
    }
    else ret =1;
    call_state_free(&state);
    return ret;
}

int
efile_symlink(Efile_error* errInfo, char* old, char* new)
{
    Efile_call_state state;
    int ret;
    DBG_TRACE2(1, "symlink(%s <- %s)", old, new);
    call_state_init(&state, errInfo);
    ret = do_symlink(&state, old, new);
    call_state_free(&state);
    return ret;
}

static int
do_symlink(Efile_call_state* state, char* old, char* new)
{
    /*
     * Load dll and see if we have CreateSymbolicLink at runtime:
     * (Vista only)
     */
    HINSTANCE hModule = NULL;
    WCHAR *wold = (WCHAR *) old;
    WCHAR *wnew = (WCHAR *) new;

    DBG_TRACE(1, old);
    if ((hModule = LoadLibrary("kernel32.dll")) != NULL) {
	typedef BOOLEAN (WINAPI  * CREATESYMBOLICLINKFUNCPTR) (
	     LPCWSTR lpSymlinkFileName,
	     LPCWSTR lpTargetFileName,
	     DWORD dwFlags);

	CREATESYMBOLICLINKFUNCPTR pCreateSymbolicLink =
	    (CREATESYMBOLICLINKFUNCPTR) GetProcAddress(hModule,
						       "CreateSymbolicLinkW");
	/* A for MBCS, W for UNICODE... char* above implies 'W'! */
	if (pCreateSymbolicLink != NULL) {
	  ensure_wpath(state, &wold);
	  ensure_wpath(state, &wnew);
	  {
	    DWORD attr = GetFileAttributesW(wold);
	    int flag = (attr != INVALID_FILE_ATTRIBUTES &&
			attr & FILE_ATTRIBUTE_DIRECTORY) ? 1 : 0;
	    /*  SYMBOLIC_LINK_FLAG_DIRECTORY = 1 */
	    BOOLEAN success = pCreateSymbolicLink(wnew, wold, flag);
	    FreeLibrary(hModule);

	    if (success) {
		return 1;
	    } else {
		return set_error(state->errInfo);
	    }
	  }
	} else
	    FreeLibrary(hModule);
    }
    errno = ENOTSUP;
    return check_error(-1, state->errInfo);
}

int
efile_fadvise(Efile_error* errInfo, int fd, Sint64 offset,
	    Sint64 length, int advise)
{
    DBG_TRACE(2, L"");
    /* posix_fadvise is not available on Windows, do nothing */
    errno = ERROR_SUCCESS;
    return check_error(0, errInfo);
}

int
efile_fallocate(Efile_error* errInfo, int fd, Sint64 offset, Sint64 length)
{
    DBG_TRACE(2, L"");
    /* No file preallocation method available in Windows. */
    errno = errno_map(ERROR_NOT_SUPPORTED);
    SetLastError(ERROR_NOT_SUPPORTED);

    return check_error(-1, errInfo);
}
