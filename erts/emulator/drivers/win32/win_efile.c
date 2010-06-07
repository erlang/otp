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
 * Purpose: Provides file and directory operations for Windows.
 */

#include <windows.h>
#include "sys.h"
#include <ctype.h>

#include "erl_efile.h"

/*
 * Microsoft-specific function to map a WIN32 error code to a Posix errno.
 */

#define ISSLASH(a)  ((a) == '\\' || (a) == '/')

#define ISDIR(st) (((st).st_mode&S_IFMT) == S_IFDIR)
#define ISREG(st) (((st).st_mode&S_IFMT) == S_IFREG)

#define IS_DOT_OR_DOTDOT(s) \
    (s[0] == '.' && (s[1] == '\0' || (s[1] == '.' && s[2] == '\0')))

#ifndef INVALID_FILE_ATTRIBUTES
#define INVALID_FILE_ATTRIBUTES ((DWORD) 0xFFFFFFFF)
#endif

static int check_error(int result, Efile_error* errInfo);
static int set_error(Efile_error* errInfo);
static int IsRootUNCName(const char* path);
static int extract_root(char* name);
static unsigned short dos_to_posix_mode(int attr, const char *name);

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
	return 0;
    }
    return 1;
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
    errInfo->posix_errno = errno_map(errInfo->os_errno = GetLastError());
    return 0;
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



int
efile_mkdir(errInfo, name)
Efile_error* errInfo;		/* Where to return error codes. */
char* name;			/* Name of directory to create. */
{
    return check_error(mkdir(name), errInfo);
}

int
efile_rmdir(errInfo, name)
Efile_error* errInfo;		/* Where to return error codes. */
char* name;			/* Name of directory to delete. */
{
    OSVERSIONINFO os;
    DWORD attr;

    if (RemoveDirectory(name) != FALSE) {
	return 1;
    }
    errno = errno_map(GetLastError());
    if (errno == EACCES) {
	attr = GetFileAttributes(name);
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
		WIN32_FIND_DATA data;
		char buffer[2*MAX_PATH];
		int len;

		len = strlen(name);
		strcpy(buffer, name);
		if (buffer[0] && buffer[len-1] != '\\' && buffer[len-1] != '/') {
		    strcat(buffer, "\\");
		}
		strcat(buffer, "*.*");
		handle = FindFirstFile(buffer, &data);
		if (handle != INVALID_HANDLE_VALUE) {
		    while (1) {
			if ((strcmp(data.cFileName, ".") != 0)
				&& (strcmp(data.cFileName, "..") != 0)) {
			    /*
			     * Found something in this directory.
			     */

			    errno = EEXIST;
			    break;
			}
			if (FindNextFile(handle, &data) == FALSE) {
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
    return check_error(-1, errInfo);
}

int
efile_delete_file(errInfo, name)
Efile_error* errInfo;		/* Where to return error codes. */
char* name;			/* Name of file to delete. */
{
    DWORD attr;

    if (DeleteFile(name) != FALSE) {
	return 1;
    }

    errno = errno_map(GetLastError());
    if (errno == EACCES) {
        attr = GetFileAttributes(name);
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
        attr = GetFileAttributes(name);
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

    return check_error(-1, errInfo);
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
efile_rename(errInfo, src, dst)
Efile_error* errInfo;		/* Where to return error codes. */
char* src;			/* Original name. */
char* dst;			/* New name. */
{
    DWORD srcAttr, dstAttr;
    
    if (MoveFile(src, dst) != FALSE) {
	return 1;
    }

    errno = errno_map(GetLastError());
    srcAttr = GetFileAttributes(src);
    dstAttr = GetFileAttributes(dst);
    if (srcAttr == (DWORD) -1) {
	srcAttr = 0;
    }
    if (dstAttr == (DWORD) -1) {
	dstAttr = 0;
    }

    if (errno == EBADF) {
	errno = EACCES;
	return check_error(-1, errInfo);
    }
    if (errno == EACCES) {
	decode:
	if (srcAttr & FILE_ATTRIBUTE_DIRECTORY) {
	    char srcPath[MAX_PATH], dstPath[MAX_PATH];
	    char *srcRest, *dstRest;
	    int size;

	    size = GetFullPathName(src, sizeof(srcPath), srcPath, &srcRest);
	    if ((size == 0) || (size > sizeof(srcPath))) {
		return check_error(-1, errInfo);
	    }
	    size = GetFullPathName(dst, sizeof(dstPath), dstPath, &dstRest);
	    if ((size == 0) || (size > sizeof(dstPath))) {
		return check_error(-1, errInfo);
	    }
	    if (srcRest == NULL) {
		srcRest = srcPath + strlen(srcPath);
	    }
	    if (strnicmp(srcPath, dstPath, srcRest - srcPath) == 0) {
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
	    if (dstPath[0] == '\0') {
		/*
		 * The filename was invalid.  (Don't know why,
		 * but play it safe.)
		 */
		errno = EINVAL;
	    }
	    if (stricmp(srcPath, dstPath) != 0) {
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

		if (RemoveDirectory(dst)) {
		    /*
		     * Now that that empty directory is gone, we can try
		     * renaming again.  If that fails, we'll put this empty
		     * directory back, for completeness.
		     */

		    if (MoveFile(src, dst) != FALSE) {
			return 1;
		    }

		    /*
		     * Some new error has occurred.  Don't know what it
		     * could be, but report this one.
		     */

		    errno = errno_map(GetLastError());
		    CreateDirectory(dst, NULL);
		    SetFileAttributes(dst, dstAttr);
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

		char tempName[MAX_PATH];
		int result, size;
		char *rest;
		
		size = GetFullPathName(dst, sizeof(tempName), tempName, &rest);
		if ((size == 0) || (size > sizeof(tempName)) || (rest == NULL)) {
		    return check_error(-1, errInfo);
		}
		*rest = '\0';
		result = -1;
		if (GetTempFileName(tempName, "erlr", 0, tempName) != 0) {
		    /*
		     * Strictly speaking, need the following DeleteFile and
		     * MoveFile to be joined as an atomic operation so no
		     * other app comes along in the meantime and creates the
		     * same temp file.
		     */
		     
		    DeleteFile(tempName);
		    if (MoveFile(dst, tempName) != FALSE) {
			if (MoveFile(src, dst) != FALSE) {
			    SetFileAttributes(tempName, FILE_ATTRIBUTE_NORMAL);
			    DeleteFile(tempName);
			    return 1;
			} else {
			    DeleteFile(dst);
			    MoveFile(tempName, dst);
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
    return check_error(-1, errInfo);
}

int
efile_chdir(errInfo, name)
Efile_error* errInfo;		/* Where to return error codes. */
char* name;			/* Name of directory to make current. */
{
    int success = check_error(chdir(name), errInfo);
    if (!success && errInfo->posix_errno == EINVAL)
	/* POSIXification of errno */
	errInfo->posix_errno = ENOENT;
    return success;
}

int
efile_getdcwd(errInfo, drive, buffer, size)
Efile_error* errInfo;		/* Where to return error codes. */
int drive;			/* 0 - current, 1 - A, 2 - B etc. */
char* buffer;			/* Where to return the current directory. */
size_t size;			/* Size of buffer. */
{
    if (_getdcwd(drive, buffer, size) == NULL)
	return check_error(-1, errInfo);
    for ( ; *buffer; buffer++) 
	if (*buffer == '\\')
	    *buffer = '/';
    return 1;
}

int
efile_readdir(errInfo, name, dir_handle, buffer, size)
Efile_error* errInfo;		/* Where to return error codes. */
char* name;			/* Name of directory to open. */
EFILE_DIR_HANDLE* dir_handle;	/* Directory handle of open directory. */
char* buffer;			/* Pointer to buffer for one filename. */
size_t size;			/* Size of buffer. */
{
    HANDLE dir;			/* Handle to directory. */
    char wildcard[MAX_PATH];	/* Wildcard to search for. */
    WIN32_FIND_DATA findData;	/* Data found by FindFirstFile() or FindNext(). */

    /*
     * First time we must setup everything.
     */

    if (*dir_handle == NULL) {
	int length = strlen(name);
	char* s;

	if (length+3 >= MAX_PATH) {
	    errno = ENAMETOOLONG;
	    return check_error(-1, errInfo);
	}

	strcpy(wildcard, name);
	s = wildcard+length-1;
	if (*s != '/' && *s != '\\')
	    *++s = '\\';
	*++s = '*';
	*++s = '\0';
	DEBUGF(("Reading %s\n", wildcard));
	dir = FindFirstFile(wildcard, &findData);
	if (dir == INVALID_HANDLE_VALUE)
	    return set_error(errInfo);
	*dir_handle = (EFILE_DIR_HANDLE) dir;

	if (!IS_DOT_OR_DOTDOT(findData.cFileName)) {
	    strcpy(buffer, findData.cFileName);
	    return 1;
	}
    }


    /*
     * Retrieve the name of the next file using the directory handle.
     */

    dir = (HANDLE) *dir_handle;

    for (;;) {
	if (FindNextFile(dir, &findData)) {
	    if (IS_DOT_OR_DOTDOT(findData.cFileName))
		continue;
	    strcpy(buffer, findData.cFileName);
	    return 1;
	}

	if (GetLastError() == ERROR_NO_MORE_FILES) {
	    FindClose(dir);
	    errInfo->posix_errno = errInfo->os_errno = 0;
	    return 0;
	}

	set_error(errInfo);
	FindClose(dir);
	return 0;
    }
}

int
efile_openfile(errInfo, name, flags, pfd, pSize)
Efile_error* errInfo;		/* Where to return error codes. */
char* name;			/* Name of directory to open. */
int flags;			/* Flags to use for opening. */
int* pfd;			/* Where to store the file descriptor. */
Sint64* pSize;			/* Where to store the size of the file. */
{
    BY_HANDLE_FILE_INFORMATION fileInfo; /* File information from a handle. */
    HANDLE fd;			/* Handle to open file. */
    DWORD access;		/* Access mode: GENERIC_READ, GENERIC_WRITE. */
    DWORD crFlags;

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

    if (flags & EFILE_MODE_APPEND) {
	crFlags = OPEN_ALWAYS;
    }
    if (flags & EFILE_MODE_EXCL) {
	crFlags = CREATE_NEW;
    }
    fd = CreateFile(name, access,
		    FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
		    NULL, crFlags, FILE_ATTRIBUTE_NORMAL, NULL);

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
	    (attr = GetFileAttributes(name)) != INVALID_FILE_ATTRIBUTES && 
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
efile_may_openfile(Efile_error* errInfo, char *name) {
    DWORD attr;

    if ((attr = GetFileAttributes(name)) == INVALID_FILE_ATTRIBUTES) {
	return check_error(-1, errInfo);
    }

    if (attr & FILE_ATTRIBUTE_DIRECTORY) {
	errno = EISDIR;
	return check_error(-1, errInfo);
    }
    return 1;
#if 0
    struct stat statbuf;
    
    if (stat(name, &statbuf)) {
	return check_error(-1, errInfo);
    }
    if (ISDIR(statbuf)) {
	errno = EISDIR;
	return check_error(-1, errInfo);
    }
    return 1;
#endif
}

void
efile_closefile(fd)
int fd;				/* File descriptor for file to close. */
{
    CloseHandle((HANDLE) fd);
}

int
efile_fdatasync(errInfo, fd)
Efile_error* errInfo;		/* Where to return error codes. */
int fd;				/* File descriptor for file to sync. */
{
    /* Not available in Windows, just call regular fsync */
    return efile_fsync(errInfo, fd);
}

int
efile_fsync(errInfo, fd)
Efile_error* errInfo;		/* Where to return error codes. */
int fd;				/* File descriptor for file to sync. */
{
    if (!FlushFileBuffers((HANDLE) fd)) {
	return check_error(-1, errInfo);
    }
    return 1;
}

int
efile_fileinfo(Efile_error* errInfo, Efile_info* pInfo,
	       char* orig_name, int info_for_link)
{
    HANDLE findhandle;		/* Handle returned by FindFirstFile(). */
    WIN32_FIND_DATA findbuf;	/* Data return by FindFirstFile(). */
    char name[_MAX_PATH];
    int name_len;
    char* path;
    char pathbuf[_MAX_PATH];
    int drive;			/* Drive for filename (1 = A:, 2 = B: etc). */

    /* Don't allow wildcards to be interpreted by system */

    if (strpbrk(orig_name, "?*")) {
    enoent:
	errInfo->posix_errno = ENOENT;
	errInfo->os_errno = ERROR_FILE_NOT_FOUND;
        return 0;
    }

    /*
     * Move the name to a buffer and make sure to remove a trailing
     * slash, because it causes FindFirstFile() to fail on Win95.
     */

    if ((name_len = strlen(orig_name)) >= _MAX_PATH) {
	goto enoent;
    } else {
	strcpy(name, orig_name);
	if (name_len > 2 && ISSLASH(name[name_len-1]) &&
	    name[name_len-2] != ':') {
	    name[name_len-1] = '\0';
	}
    }
    
    /* Try to get disk from name.  If none, get current disk.  */

    if (name[1] != ':') {
        drive = 0;
        if (GetCurrentDirectory(sizeof(pathbuf), pathbuf) &&
	    pathbuf[1] == ':') {
	    drive = tolower(pathbuf[0]) - 'a' + 1;
	}
    } else if (*name && name[2] == '\0') {
	/*
	 * X: and nothing more is an error.
	 */
	errInfo->posix_errno = ENOENT;
	errInfo->os_errno = ERROR_FILE_NOT_FOUND;
	return 0;
    } else
        drive = tolower(*name) - 'a' + 1;

    findhandle = FindFirstFile(name, &findbuf);
    if (findhandle == INVALID_HANDLE_VALUE) {
        if (!(strpbrk(name, "./\\") &&
	      (path = _fullpath(pathbuf, name, _MAX_PATH)) &&
	      /* root dir. ('C:\') or UNC root dir. ('\\server\share\') */
	      ((strlen(path) == 3) || IsRootUNCName(path)) &&
	      (GetDriveType(path) > 1)   ) ) {
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
        findbuf.cFileName[0] = '\0';

	pInfo->modifyTime.year = 1980;
	pInfo->modifyTime.month = 1;
	pInfo->modifyTime.day = 1;
	pInfo->modifyTime.hour = 0;
	pInfo->modifyTime.minute = 0;
	pInfo->modifyTime.second = 0;

	pInfo->accessTime = pInfo->modifyTime;
    } else {
	SYSTEMTIME SystemTime;
        FILETIME LocalFTime;

#define GET_TIME(dst, src) \
if (!FileTimeToLocalFileTime(&findbuf.src, &LocalFTime) || \
    !FileTimeToSystemTime(&LocalFTime, &SystemTime)) { \
    return set_error(errInfo); \
} \
(dst).year = SystemTime.wYear; \
(dst).month = SystemTime.wMonth; \
(dst).day = SystemTime.wDay; \
(dst).hour = SystemTime.wHour; \
(dst).minute = SystemTime.wMinute; \
(dst).second = SystemTime.wSecond;

        GET_TIME(pInfo->modifyTime, ftLastWriteTime);

        if (findbuf.ftLastAccessTime.dwLowDateTime == 0 &&
	    findbuf.ftLastAccessTime.dwHighDateTime == 0) {
	    pInfo->accessTime = pInfo->modifyTime;
	} else {
	    GET_TIME(pInfo->accessTime, ftLastAccessTime);
	}

        if (findbuf.ftCreationTime.dwLowDateTime == 0 &&
	    findbuf.ftCreationTime.dwHighDateTime == 0) {
	    pInfo->cTime = pInfo->modifyTime;
	} else {
	    GET_TIME(pInfo->cTime, ftCreationTime);
	}
#undef GET_TIME
        FindClose(findhandle);
    }

    pInfo->size_low = findbuf.nFileSizeLow;
    pInfo->size_high = findbuf.nFileSizeHigh;
	
    if (findbuf.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
	pInfo->type = FT_DIRECTORY;
    else
	pInfo->type = FT_REGULAR;

    if (findbuf.dwFileAttributes & FILE_ATTRIBUTE_READONLY)
	pInfo->access = FA_READ;
    else
	pInfo->access = FA_READ|FA_WRITE;

    pInfo->mode = dos_to_posix_mode(findbuf.dwFileAttributes, name);
    pInfo->links = 1;
    pInfo->major_device = drive;
    pInfo->minor_device = 0;
    pInfo->inode = 0;
    pInfo->uid = 0;
    pInfo->gid = 0;
    
    return 1;
}

int
efile_write_info(errInfo, pInfo, name)
Efile_error* errInfo;
Efile_info* pInfo;
char* name;
{
    SYSTEMTIME timebuf;
    FILETIME LocalFileTime;
    FILETIME ModifyFileTime;
    FILETIME AccessFileTime;
    FILETIME CreationFileTime;
    HANDLE fd;
    FILETIME* mtime = NULL;
    FILETIME* atime = NULL;
    FILETIME* ctime = NULL;
    DWORD attr;
    DWORD tempAttr;
    BOOL modifyTime = FALSE;

    /*
     * Get the attributes for the file.
     */

    tempAttr = attr = GetFileAttributes((LPTSTR)name);
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

#define MKTIME(tb, ts, ptr) \
    timebuf.wYear = ts.year; \
    timebuf.wMonth = ts.month; \
    timebuf.wDay = ts.day; \
    timebuf.wHour = ts.hour; \
    timebuf.wMinute = ts.minute; \
    timebuf.wSecond = ts.second; \
    timebuf.wMilliseconds = 0; \
    if (ts.year != -1) { \
      modifyTime = TRUE; \
      ptr = &tb; \
      if (!SystemTimeToFileTime(&timebuf, &LocalFileTime ) || \
	!LocalFileTimeToFileTime(&LocalFileTime, &tb)) { \
        errno = EINVAL; \
	return check_error(-1, errInfo); \
     } \
    }

    MKTIME(ModifyFileTime, pInfo->accessTime, mtime);
    MKTIME(AccessFileTime, pInfo->modifyTime, atime);
    MKTIME(CreationFileTime, pInfo->cTime, ctime);
#undef MKTIME

    /*
     * If necessary, set the file times.
     */

    if (modifyTime) {
	/*
	 * If the has read only access, we must temporarily turn on
	 * write access (this is necessary for native filesystems,
	 * but not for NFS filesystems).
	 */

	if (tempAttr & FILE_ATTRIBUTE_READONLY) {
	    tempAttr &= ~FILE_ATTRIBUTE_READONLY;
	    if (!SetFileAttributes((LPTSTR) name, tempAttr)) {
		return set_error(errInfo);
	    }
	}

	fd = CreateFile(name, GENERIC_READ|GENERIC_WRITE,
			FILE_SHARE_READ | FILE_SHARE_WRITE,
			NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
	if (fd != INVALID_HANDLE_VALUE) {
	    BOOL result = SetFileTime(fd, ctime, atime, mtime);
	    if (!result) {
		return set_error(errInfo);
	    }
	    CloseHandle(fd);
	}
    }

    /*
     * If the file doesn't have the correct attributes, set them now.
     * (It could have been done before setting the file times, above).
     */

    if (tempAttr != attr) {
	if (!SetFileAttributes((LPTSTR) name, attr)) {
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
    int res  = efile_seek(errInfo, fd, offset, EFILE_SEEK_SET, NULL);
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
    int res = efile_seek(errInfo, fd, offset, EFILE_SEEK_SET, NULL);
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

    if (flags & EFILE_MODE_APPEND) {
	(void) SetFilePointer((HANDLE) fd, 0, NULL, FILE_END);
    }
    while (count > 0) {
	if (!WriteFile((HANDLE) fd, buf, count, &written, NULL))
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
	     int iovcnt,             /* Number of structs in vector */
	     size_t size)            /* Number of bytes to write */
{
    int cnt;                         /* Buffers so far written */

    ASSERT(iovcnt >= 0);
    
    if (flags & EFILE_MODE_APPEND) {
	(void) SetFilePointer((HANDLE) fd, 0, NULL, FILE_END);
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
			       NULL))
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
    if (!ReadFile((HANDLE) fd, buf, count, (DWORD *) pBytesRead, NULL))
	return set_error(errInfo);
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
    if (!SetEndOfFile((HANDLE) (*fd)))
	return set_error(errInfo);
    return 1;
}


/*
 * IsRootUNCName - returns TRUE if the argument is a UNC name specifying
 *      a root share.  That is, if it is of the form \\server\share\.
 *      This routine will also return true if the argument is of the
 *      form \\server\share (no trailing slash) but Win32 currently
 *      does not like that form.
 *
 *      Forward slashes ('/') may be used instead of backslashes ('\').
 */

static int
IsRootUNCName(const char* path)
{
    /*
     * If a root UNC name, path will start with 2 (but not 3) slashes
     */

    if ((strlen(path) >= 5) /* minimum string is "//x/y" */
	&& ISSLASH(path[0]) && ISSLASH(path[1]))
    {
        const char * p = path + 2 ;

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
extract_root(char* name)
{
    int len = strlen(name);

    if (isalpha(name[0]) && name[1] == ':' && ISSLASH(name[2])) {
	int c = name[3];
	name[3] = '\0';
	return c == '\0';
    } else if (len < 5 || !ISSLASH(name[0]) || !ISSLASH(name[1])) {
	goto error;
    } else {			/* Try to find the end of the UNC name. */
	char* p;
	int c;

        /*
         * Find the slash between the server name and share name.
         */

	for (p = name + 2; *p; p++)
            if (ISSLASH(*p))
                break;
	if (*p == '\0')
	    goto error;

	/*
	 * Find the slash after the share name.
	 */

	for (p++; *p; p++)
            if (ISSLASH(*p))
                break;
	c = *p;
	*p = '\0';
	return c == '\0' || p[1] == '\0';
    }

 error:
    *name = '\0';
    return 1;
}

static unsigned short
dos_to_posix_mode(int attr, const char *name)
{
    register unsigned short uxmode;
    unsigned dosmode;
    register const char *p;

    dosmode = attr & 0xff;
    if ((p = name)[1] == ':')
	p += 2;

    /* check to see if this is a directory - note we must make a special
     * check for the root, which DOS thinks is not a directory
     */

    uxmode = (unsigned short)
	(((ISSLASH(*p) && !p[1]) || (dosmode & FILE_ATTRIBUTE_DIRECTORY) ||
	       *p == '\0') ? _S_IFDIR|_S_IEXEC : _S_IFREG);

    /* If attribute byte does not have read-only bit, it is read-write */

    uxmode |= (dosmode & FILE_ATTRIBUTE_READONLY) ?
	_S_IREAD : (_S_IREAD|_S_IWRITE);

    /* see if file appears to be executable - check extension of name */

    if (p = strrchr(name, '.')) {
        if (!stricmp(p, ".exe") ||
	    !stricmp(p, ".cmd") ||
	    !stricmp(p, ".bat") ||
	    !stricmp(p, ".com"))
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
    errno = ENOTSUP;
    return check_error(-1, errInfo);
}


int
efile_altname(Efile_error* errInfo, char* orig_name, char* buffer, size_t size)
{
    WIN32_FIND_DATA wfd;
    HANDLE fh;
    char name[_MAX_PATH];
    int name_len;
    char* path;
    char pathbuf[_MAX_PATH];
    int drive;			/* Drive for filename (1 = A:, 2 = B: etc). */

    /* Don't allow wildcards to be interpreted by system */

    if (strpbrk(orig_name, "?*")) {
    enoent:
	errInfo->posix_errno = ENOENT;
	errInfo->os_errno = ERROR_FILE_NOT_FOUND;
        return 0;
    }

    /*
     * Move the name to a buffer and make sure to remove a trailing
     * slash, because it causes FindFirstFile() to fail on Win95.
     */

    if ((name_len = strlen(orig_name)) >= _MAX_PATH) {
	goto enoent;
    } else {
	strcpy(name, orig_name);
	if (name_len > 2 && ISSLASH(name[name_len-1]) &&
	    name[name_len-2] != ':') {
	    name[name_len-1] = '\0';
	}
    }
    
    /* Try to get disk from name.  If none, get current disk.  */

    if (name[1] != ':') {
        drive = 0;
        if (GetCurrentDirectory(sizeof(pathbuf), pathbuf) &&
	    pathbuf[1] == ':') {
	    drive = tolower(pathbuf[0]) - 'a' + 1;
	}
    } else if (*name && name[2] == '\0') {
	/*
	 * X: and nothing more is an error.
	 */
	goto enoent;
    } else {
        drive = tolower(*name) - 'a' + 1;
    }
    fh = FindFirstFile(name,&wfd);
    if (fh == INVALID_HANDLE_VALUE) {
        if (!(strpbrk(name, "./\\") &&
	      (path = _fullpath(pathbuf, name, _MAX_PATH)) &&
	      /* root dir. ('C:\') or UNC root dir. ('\\server\share\') */
	      ((strlen(path) == 3) || IsRootUNCName(path)) &&
	      (GetDriveType(path) > 1)   ) ) {
	    errno = errno_map(GetLastError());
	    return check_error(-1, errInfo);
	}
        /*
         * Root directories (such as C:\ or \\server\share\ are fabricated.
         */
	strcpy(buffer,name);
	return 1;
    }
	
    strcpy(buffer,wfd.cAlternateFileName);
    if (!*buffer) {
	strcpy(buffer,wfd.cFileName);
    }

    return 1;
}

int
efile_link(Efile_error* errInfo, char* old, char* new)
{
    errno = ENOTSUP;
    return check_error(-1, errInfo);
}

int
efile_symlink(Efile_error* errInfo, char* old, char* new)
{
    errno = ENOTSUP;
    return check_error(-1, errInfo);
}

int
efile_fadvise(Efile_error* errInfo, int fd, Sint64 offset,
	    Sint64 length, int advise)
{
    /* posix_fadvise is not available on Windows, do nothing */
    errno = ERROR_SUCCESS;
    return check_error(0, errInfo);
}
