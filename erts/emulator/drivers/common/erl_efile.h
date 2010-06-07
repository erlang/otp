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
 * Defines the interfaces between the generic efile driver and its
 * operating-system dependent helpers.
 */

#include "sys.h"
#include "erl_driver.h"

/*
 * Open modes for efile_openfile().
 */
#define EFILE_MODE_READ 1
#define EFILE_MODE_WRITE 2	/* Implies truncating file when used alone. */
#define EFILE_MODE_READ_WRITE 	3
#define EFILE_MODE_APPEND	4
#define EFILE_COMPRESSED 	8
#define EFILE_MODE_EXCL        16
#define EFILE_NO_TRUNCATE      32 /* Special for reopening on VxWorks */

/*
 * Seek modes for efile_seek().
 */
#define	EFILE_SEEK_SET	0
#define	EFILE_SEEK_CUR	1
#define	EFILE_SEEK_END	2

/*
 * File types returned by efile_fileinfo().
 */
#define FT_DEVICE 	1
#define FT_DIRECTORY 	2
#define FT_REGULAR	3
#define FT_SYMLINK	4
#define FT_OTHER	5

/*
 * Access attributes returned by efile_fileinfo() (the bits can be ORed
 * together).
 */
#define FA_NONE    	0
#define FA_WRITE 	1
#define FA_READ		2

/*
 * An handle to an open directory.  To be cast to the correct type
 * in the system-dependent directory functions.
 */

typedef struct _Efile_Dir_Handle* EFILE_DIR_HANDLE;

/*
 * Error information from the last call.
 */  
typedef struct _Efile_error {
    int posix_errno;		/* Posix error number, as in <errno.h>. */
    int os_errno;		/* Os-dependent error number (not used). */
} Efile_error;

/*
 * This structure contains date and time.
 */
typedef struct _Efile_time {
    unsigned year;		/* (4 digits). */
    unsigned month;		/* (1..12). */
    unsigned day;		/* (1..31). */
    unsigned hour;		/* (0..23). */
    unsigned minute;		/* (0..59). */
    unsigned second;		/* (0..59). */
} Efile_time;


/*
 * Describes what is returned by file:file_info/1.
 */

typedef struct _Efile_info {
    Uint32 size_low;		/* Size of file, lower 32 bits.. */
    Uint32 size_high;		/* Size of file, higher 32 bits. */
    Uint32 type;		/* Type of file -- one of FT_*. */
    Uint32 access;		/* Access to file -- one of FA_*. */
    Uint32 mode;		/* Access permissions -- bit field. */
    Uint32 links;		/* Number of links to file. */
    Uint32 major_device;	/* Major device or file system. */
    Uint32 minor_device;	/* Minor device (for devices). */
    Uint32 inode;		/* Inode number. */
    Uint32 uid;			/* User id of owner. */
    Uint32 gid;			/* Group id of owner. */
    Efile_time accessTime;	/* Last time the file was accessed. */
    Efile_time modifyTime;	/* Last time the file was modified. */
    Efile_time cTime;		/* Creation time (Windows) or last
				 * inode change (Unix).
				 */
} Efile_info;

/*
 * Functions.
 */

int efile_mkdir(Efile_error* errInfo, char* name);
int efile_rmdir(Efile_error* errInfo, char* name);
int efile_delete_file(Efile_error* errInfo, char* name);
int efile_rename(Efile_error* errInfo, char* src, char* dst);
int efile_chdir(Efile_error* errInfo, char* name);
int efile_getdcwd(Efile_error* errInfo, int drive,
		  char* buffer, size_t size);
int efile_readdir(Efile_error* errInfo, char* name, 
		  EFILE_DIR_HANDLE* dir_handle,
		  char* buffer, size_t size);
int efile_openfile(Efile_error* errInfo, char* name, int flags,
		   int* pfd, Sint64* pSize);
void efile_closefile(int fd);
int efile_fdatasync(Efile_error* errInfo, int fd);
int efile_fsync(Efile_error* errInfo, int fd);
int efile_fileinfo(Efile_error* errInfo, Efile_info* pInfo,
		   char *name, int info_for_link);
int efile_write_info(Efile_error* errInfo, Efile_info* pInfo, char *name);
int efile_write(Efile_error* errInfo, int flags, int fd, 
		char* buf, size_t count);
int efile_writev(Efile_error* errInfo, int flags, int fd, 
		 SysIOVec* iov, int iovcnt, size_t size);
int efile_read(Efile_error* errInfo, int flags, int fd, 
	       char* buf, size_t count, size_t* pBytesRead);
int efile_seek(Efile_error* errInfo, int fd, 
	       Sint64 offset, int origin, Sint64* new_location);
int efile_truncate_file(Efile_error* errInfo, int *fd, int flags);
int efile_pwrite(Efile_error* errInfo, int fd, 
		 char* buf, size_t count, Sint64 offset);
int efile_pread(Efile_error* errInfo, int fd, 
		Sint64 offset, char* buf, size_t count, size_t* pBytesRead);
int efile_readlink(Efile_error* errInfo, char *name, 
		   char* buffer, size_t size);
int efile_altname(Efile_error* errInfo, char *name, 
		  char* buffer, size_t size);
int efile_link(Efile_error* errInfo, char* old, char* new);
int efile_symlink(Efile_error* errInfo, char* old, char* new);
int efile_may_openfile(Efile_error* errInfo, char *name);
int efile_fadvise(Efile_error* errInfo, int fd, Sint64 offset, Sint64 length,
		  int advise);
